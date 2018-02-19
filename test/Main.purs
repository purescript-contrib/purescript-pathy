module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, info)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, File, Path, Rel, canonicalize, currentDir, depth, dir, dropExtension, file, parentOf, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, renameFile', rootDir, unsafePrintPath, (<..>), (<.>), (</>))
import Data.Path.Pathy.Gen as PG
import Data.Path.Pathy.Sandboxed (printPath, sandbox, unsandbox)
import Data.String as Str
import Data.String.NonEmpty (NonEmptyString)
import Data.Symbol (SProxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol) as Symbol
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Laws.Data as Laws.Data
import Type.Data.Boolean (False) as Symbol
import Type.Data.Symbol (class Equals) as Symbol
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

test :: forall a eff. Show a => Eq a => String -> a -> a -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
test name actual expected= do
  info $ "Test: " <> name
  if expected == actual
    then info $ "Passed: " <> (show expected)
    else throw $ "Failed:\n    Expected: " <> (show expected) <> "\n    Actual:   " <> (show actual)

test' :: forall a b eff. IsRelOrAbs a => IsDirOrFile b => String -> Path a b -> String -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
test' n p s = test n (unsafePrintPath p) s

newtype ArbPath = ArbPath (Path Abs File)

derive newtype instance eqArbPath :: Eq ArbPath
derive newtype instance ordArbPath :: Ord ArbPath

runArbPath ∷ ArbPath → (Path Abs File)
runArbPath (ArbPath p) = p

instance arbitraryArbPath ∷ QC.Arbitrary ArbPath where
  arbitrary = do
    numDirs ← Gen.chooseInt 1 10
    dirs ← map dir <$> Gen.vectorOf numDirs pathPart
    filename ← file <$> pathPart
    pure $ ArbPath $ rootDir </> foldl (flip (</>)) filename (dirs ∷ Array (Path Rel Dir))

pathPart ∷ Gen.Gen NonEmptyString
pathPart = asNonEmptyString <$> Gen.suchThat QC.arbitrary (not <<< Str.null)
  where
  asNonEmptyString :: String -> NonEmptyString
  asNonEmptyString = unsafeCoerce

dirFoo :: Path Rel Dir
dirFoo = dir (reflectNonEmpty $ SProxy :: SProxy "foo")

dirBar :: Path Rel Dir
dirBar = dir (reflectNonEmpty $ SProxy :: SProxy "bar")

parsePrintCheck :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> Maybe (Path a b) -> QC.Result
parsePrintCheck input parsed =
  if parsed == Just input
    then QC.Success
    else QC.Failed
      $ "`parse (print path) != Just path` for path: `" <> show input <> "` which was re-parsed into `" <> show parsed <> "`"
      <> "\n\tPrinted path: " <> show (unsafePrintPath input)
      <> "\n\tPrinted path': `" <> show (map unsafePrintPath parsed) <> "`"

parsePrintAbsDirPath :: Gen.Gen QC.Result
parsePrintAbsDirPath = PG.genAbsDirPath <#> \path ->
  parsePrintCheck path (parseAbsDir $ unsafePrintPath path)

parsePrintAbsFilePath :: Gen.Gen QC.Result
parsePrintAbsFilePath = PG.genAbsFilePath <#> \path ->
  parsePrintCheck path (parseAbsFile $ unsafePrintPath path)

parsePrintRelDirPath :: Gen.Gen QC.Result
parsePrintRelDirPath = PG.genRelDirPath <#> \path ->
  parsePrintCheck path (parseRelDir $ unsafePrintPath path)

parsePrintRelFilePath :: Gen.Gen QC.Result
parsePrintRelFilePath = PG.genRelFilePath <#> \path ->
  parsePrintCheck path (parseRelFile $ unsafePrintPath path)

main :: QC.QC () Unit
main = do
  info "checking `parse <<< print` for `AbsDir`" *> QC.quickCheck parsePrintAbsDirPath
  info "checking `parse <<< print` for `AbsFile`" *> QC.quickCheck parsePrintAbsFilePath
  info "checking `parse <<< print` for `RelDir`" *> QC.quickCheck parsePrintRelDirPath
  info "checking `parse <<< print` for `RelFile`" *> QC.quickCheck parsePrintRelFilePath

  -- Should not compile:
  -- test
  --   "(</>) - file in dir"
  --   (printPath (file "image.png" </> dirFoo))
  --   "./image.png/foo"

  -- Should not compile:
  -- test
  --   "(</>) - absolute dir in absolute dir"
  --   (printPath (rootDir </> rootDir))
  --   "/"

  -- Should not compile:
  -- test
  --   "(</>) - absolute dir in relative dir"
  --   (printPath (currentDir </> rootDir))
  --   "/"

  test' "(</>) - two directories"
    (dirFoo </> dirBar)
    "./foo/bar/"

  test' "(</>) - file with two parents"
    (dirFoo
      </> dirBar
      </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))
    "./foo/bar/image.png"

  test' "(<.>) - file without extension"
    (file (reflectNonEmpty $ SProxy :: SProxy "image")
      <.> (reflectNonEmpty $ SProxy :: SProxy "png"))
    "./image.png"

  test' "(<.>) - file with extension"
    (file (reflectNonEmpty $ SProxy :: SProxy "image.jpg")
      <.> (reflectNonEmpty $ SProxy :: SProxy "png"))
    "./image.png"

  test' "printPath - ./../"
    (parentOf currentDir)
    "./../"

  test' "(</>) - ./../foo/"
    (parentOf currentDir </> dirFoo)
    "./../foo/"

  test' "parentOf - ./../foo/../"
    ((parentOf currentDir </> dirFoo) </> (parentOf currentDir))
    "./../foo/../"

  test' "(<..>) - ./../"
    (currentDir <..> currentDir)
    "./../"

  test' "(<..>) - ./../foo/"
    (currentDir <..> dirFoo)
    "./../foo/"

  test' "(<..>) - ./../foo/../"
    ((currentDir <..> dirFoo) <..> currentDir)
    "./../foo/../"

  test' "canonicalize - 1 down, 1 up"
    (canonicalize $ parentOf $ dirFoo)
    "./"

  test' "canonicalize - 2 down, 2 up"
    (canonicalize (parentOf (parentOf (dirFoo </> dirBar))))
    "./"

  test "renameFile - single level deep"
    (renameFile' dropExtension (file (reflectNonEmpty $ SProxy :: SProxy "image.png")))
    (Just $ file $ reflectNonEmpty $ SProxy :: SProxy "image")

  test "sandbox - fail when relative path lies outside sandbox (above)"
    (sandbox (rootDir </> dirBar) (parentOf currentDir))
    Nothing

  test "sandbox - fail when relative path lies outside sandbox (neigbouring)"
    (sandbox (rootDir </> dirBar) (parentOf currentDir </> dirFoo))
    Nothing

  test "sandbox - fail when absolute path lies outside sandbox"
    (sandbox (rootDir </> dirBar) (rootDir </> dirFoo </> dirBar))
    Nothing

  test "sandbox - succeed when relative path goes above sandbox but returns to it"
    (unsandbox <$> sandbox (rootDir </> dirBar) (parentOf currentDir </> dirBar))
    (Just (parentOf currentDir </> dirBar))

  test "sandbox - succeed when absolute path lies inside sandbox"
    (unsandbox <$> sandbox (rootDir </> dirBar) (rootDir </> dirBar </> dirFoo))
    (Just (rootDir </> dirBar </> dirFoo))

  test "sandbox - print relative path that goes above sandbox but returns to it"
    (printPath <$> sandbox (rootDir </> dirBar) (parentOf currentDir </> dirBar))
    (Just "/bar/")

  test "sandbox - print absolute path that lies inside sandbox"
    (printPath <$> sandbox (rootDir </> dirBar) (rootDir </> dirBar </> dirFoo))
    (Just "/bar/foo/")

  test "depth - negative"
    (depth (parentOf $ parentOf $ parentOf $ currentDir)) (-3)

  test "parseRelFile - image.png"
    (parseRelFile "image.png")
    (Just $ file $ reflectNonEmpty $ SProxy :: SProxy "image.png")

  test "parseRelFile - ./image.png"
    (parseRelFile "./image.png")
    (Just $ file $ reflectNonEmpty $ SProxy :: SProxy "image.png")

  test "parseRelFile - foo/image.png"
    (parseRelFile "foo/image.png")
    (Just $ dirFoo </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseRelFile - ../foo/image.png"
    (parseRelFile "../foo/image.png")
    (Just $ currentDir <..> dirFoo </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseAbsFile - /image.png"
    (parseAbsFile "/image.png")
    (Just $ rootDir </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseAbsFile - /foo/image.png"
    (parseAbsFile "/foo/image.png")
    (Just $ rootDir </> dirFoo </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseRelDir - empty string"
    (parseRelDir "")
    Nothing

  test "parseRelDir - ./../"
    (parseRelDir "./../")
    (Just $ currentDir <..> currentDir)

  test "parseRelDir - foo/"
    (parseRelDir "foo/")
    (Just dirFoo)

  test "parseRelDir - foo/bar"
    (parseRelDir "foo/bar/")
    (Just $ dirFoo </> dirBar)

  test "parseRelDir - ./foo/bar"
    (parseRelDir "./foo/bar/")
    (Just $ dirFoo </> dirBar)

  test "parseAbsDir - /"
    (parseAbsDir "/")
    (Just $ rootDir)

  test "parseAbsDir - /foo/"
    (parseAbsDir "/foo/")
    (Just $ rootDir </> dirFoo)

  test "parseAbsDir - /foo/bar"
    (parseAbsDir "/foo/bar/")
    (Just $ rootDir </> dirFoo </> dirBar)

  info "Checking typeclass laws..."
  Laws.Data.checkEq (Proxy :: Proxy ArbPath)
  Laws.Data.checkOrd (Proxy :: Proxy ArbPath)

class IsSymbolNonEmpty sym where
  reflectNonEmpty :: SProxy sym -> NonEmptyString

instance isSymbolNonEmpty :: (Symbol.IsSymbol s, Symbol.Equals s "" Symbol.False) => IsSymbolNonEmpty s where
  reflectNonEmpty _ = asNonEmpty $ Symbol.reflectSymbol (SProxy :: SProxy s)
    where
    asNonEmpty :: String -> NonEmptyString
    asNonEmpty = unsafeCoerce
