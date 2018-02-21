module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, info)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.String.NonEmpty (NonEmptyString)
import Data.Symbol (class IsSymbol, reflectSymbol) as Symbol
import Data.Symbol (SProxy(..))
import Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, Path, Rel, alterExtension, canonicalize, currentDir, dir, file, parentOf, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, posixParser, posixPrinter, printPath, relativeTo, rename, rootDir, sandbox, unsafePrintPath, unsandbox, (<..>), (<.>), (</>))
import Pathy.Gen as PG
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as Gen
import Type.Data.Boolean (False) as Symbol
import Type.Data.Symbol (class Equals) as Symbol
import Unsafe.Coerce (unsafeCoerce)

test :: forall a eff. Show a => Eq a => String -> a -> a -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
test name actual expected= do
  info $ "Test: " <> name
  if expected == actual
    then info $ "Passed: " <> (show expected)
    else throw $ "Failed:\n    Expected: " <> (show expected) <> "\n    Actual:   " <> (show actual)

test' :: forall a b eff. IsRelOrAbs a => IsDirOrFile b => String -> Path a b -> String -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
test' n p s = test n (unsafePrintPath posixPrinter p) s

pathPart ∷ Gen.Gen NonEmptyString
pathPart = asNonEmptyString <$> Gen.suchThat QC.arbitrary (not <<< Str.null)
  where
  asNonEmptyString :: String -> NonEmptyString
  asNonEmptyString = unsafeCoerce

dirFoo :: Path Rel Dir
dirFoo = dir (reflectNonEmpty $ SProxy :: SProxy "foo")

dirBar :: Path Rel Dir
dirBar = dir (reflectNonEmpty $ SProxy :: SProxy "bar")

dirBaz :: Path Rel Dir
dirBaz = dir (reflectNonEmpty $ SProxy :: SProxy "baz")

parsePrintCheck :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> Maybe (Path a b) -> QC.Result
parsePrintCheck input parsed =
  if parsed == Just input
    then QC.Success
    else QC.Failed
      $ "`parse (print path) != Just path` for path: `" <> show input <> "` which was re-parsed into `" <> show parsed <> "`"
      <> "\n\tPrinted path: " <> show (unsafePrintPath posixPrinter input)
      <> "\n\tPrinted path': `" <> show (map (unsafePrintPath posixPrinter) parsed) <> "`"

parsePrintAbsDirPath :: Gen.Gen QC.Result
parsePrintAbsDirPath = PG.genAbsDirPath <#> \path ->
  parsePrintCheck path (parseAbsDir posixParser $ unsafePrintPath posixPrinter path)

parsePrintAbsFilePath :: Gen.Gen QC.Result
parsePrintAbsFilePath = PG.genAbsFilePath <#> \path ->
  parsePrintCheck path (parseAbsFile posixParser $ unsafePrintPath posixPrinter path)

parsePrintRelDirPath :: Gen.Gen QC.Result
parsePrintRelDirPath = PG.genRelDirPath <#> \path ->
  parsePrintCheck path (parseRelDir posixParser $ unsafePrintPath posixPrinter path)

parsePrintRelFilePath :: Gen.Gen QC.Result
parsePrintRelFilePath = PG.genRelFilePath <#> \path ->
  parsePrintCheck path (parseRelFile posixParser $ unsafePrintPath posixPrinter path)

checkRelative :: forall b. IsDirOrFile b => Gen.Gen (Path Abs b) -> Gen.Gen QC.Result
checkRelative gen = do
  p1 <- gen
  p2 <- PG.genAbsDirPath
  let cp1 = canonicalize p1
  let cp2 = canonicalize p2
  let rel = cp1 `relativeTo` cp2
  let cp1' = canonicalize (cp2 </> rel)
  pure
    if cp1 == cp1'
      then QC.Success
      else
        QC.Failed
          $ "`relativeTo` property did not hold:"
          <> "\n\tcp1:  " <> unsafePrintPath posixPrinter cp1
          <> "\n\tcp2:  " <> unsafePrintPath posixPrinter cp2
          <> "\n\trel:  " <> unsafePrintPath posixPrinter rel
          <> "\n\tcp1': " <> unsafePrintPath posixPrinter cp1'

main :: QC.QC () Unit
main = do
  info "checking `parse <<< print` for `AbsDir`" *> QC.quickCheck parsePrintAbsDirPath
  info "checking `parse <<< print` for `AbsFile`" *> QC.quickCheck parsePrintAbsFilePath
  info "checking `parse <<< print` for `RelDir`" *> QC.quickCheck parsePrintRelDirPath
  info "checking `parse <<< print` for `RelFile`" *> QC.quickCheck parsePrintRelFilePath
  info "checking `relativeTo` for `AbsDir`" *> QC.quickCheck (checkRelative PG.genAbsDirPath)
  info "checking `relativeTo` for `AbsFile`" *> QC.quickCheck (checkRelative PG.genAbsFilePath)

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

  test' "canonicalize - 2 up from root"
    (canonicalize (parentOf (parentOf rootDir)))
    "/"

  test "canonicalize /foo/../bar/ = /bar"
    (canonicalize (rootDir </> dirFoo <..> dirBar))
    (rootDir </> dirBar)

  test "relativeTo rootDir rootDir = currentDir"
    (relativeTo rootDir rootDir)
    (currentDir)

  test' "(rootDir </> dirFoo) `relativeTo` (rootDir </> dirFoo) = ./"
    ((rootDir </> dirFoo) `relativeTo` (rootDir </> dirFoo))
    "./"

  test' "(rootDir </> dirFoo) `relativeTo` rootDir = currentDir </> dirFoo"
    ((rootDir </> dirFoo) `relativeTo` rootDir)
    "./foo/"

  test' "(rootDir </> dirFoo) `relativeTo` (rootDir </> dirBar) = currentDir <..> dirFoo"
    ((rootDir </> dirFoo) `relativeTo` (rootDir </> dirBar))
    "./../foo/"

  test' "(rootDir </> dirBar) `relativeTo` (rootDir </> dirFoo) = ./../bar/"
    ((rootDir </> dirBar) `relativeTo` (rootDir </> dirFoo))
    "./../bar/"

  test' "(rootDir </> dirBar) `relativeTo` (rootDir </> dirFoo </> dirFoo) = ./../../bar/"
    ((rootDir </> dirBar) `relativeTo` (rootDir </> dirFoo </> dirFoo))
    "./../../bar/"

  test' "(rootDir </> dirBar) `relativeTo` (rootDir </> dirFoo </> dirFoo </> dirFoo) = ./../../../bar/"
    ((rootDir </> dirBar) `relativeTo` (rootDir </> dirFoo </> dirFoo </> dirFoo))
    "./../../../bar/"

  test' "(rootDir </> dirBar </> dirBar) `relativeTo` (rootDir </> dirFoo) = ./../bar/bar/"
    ((rootDir </> dirBar </> dirBar) `relativeTo` (rootDir </> dirFoo))
    "./../bar/bar/"

  test' "(rootDir </> dirBar </> dirBar) `relativeTo` (rootDir </> dirFoo </> dirFoo) = ./../../bar/bar/"
    ((rootDir </> dirBar </> dirBar) `relativeTo` (rootDir </> dirFoo </> dirFoo))
    "./../../bar/bar/"

  test' "(rootDir </> dirBar </> dirFoo </> dirFoo) `relativeTo` (rootDir </> dirFoo </> dirFoo </> dirFoo) = ./../../../bar/foo/foo"
    ((rootDir </> dirBar </> dirFoo </> dirFoo) `relativeTo` (rootDir </> dirFoo </> dirFoo </> dirFoo))
    "./../../../bar/foo/foo/"

  test' "(rootDir </> dirFoo </> dirBar </> dirBaz) `relativeTo` rootDir = ./foo/bar/baz/"
    ((rootDir </> dirFoo </> dirBar </> dirBaz) `relativeTo` rootDir)
    "./foo/bar/baz/"

  test' "(rootDir </> dirFoo </> dirBar </> dirBaz) `relativeTo` (rootDir </> dirFoo) = ./bar/baz/"
    ((rootDir </> dirFoo </> dirBar </> dirBaz) `relativeTo` (rootDir </> dirFoo))
    "./bar/baz/"

  test' "(rootDir </> dirFoo </> dirBar </> dirBaz) `relativeTo` (rootDir </> dirBaz) = ./../foo/bar/baz/"
    ((rootDir </> dirFoo </> dirBar </> dirBaz) `relativeTo` (rootDir </> dirBaz))
    "./../foo/bar/baz/"

  test' "(rootDir </> dirBar </> dirFoo) `relativeTo` (rootDir </> dirBar) = ./foo/"
    ((rootDir </> dirBar </> dirFoo) `relativeTo` (rootDir </> dirBar))
    "./foo/"

  test "rename - single level deep"
    (rename (alterExtension (const Nothing)) (file (reflectNonEmpty $ SProxy :: SProxy "image.png")))
    (file $ reflectNonEmpty $ SProxy :: SProxy "image")

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
    (printPath posixPrinter <$> sandbox (rootDir </> dirBar) (parentOf currentDir </> dirBar))
    (Just "/bar/")

  test "sandbox - print absolute path that lies inside sandbox"
    (printPath posixPrinter <$> sandbox (rootDir </> dirBar) (rootDir </> dirBar </> dirFoo))
    (Just "/bar/foo/")

  test "parseRelFile - image.png"
    (parseRelFile posixParser "image.png")
    (Just $ file $ reflectNonEmpty $ SProxy :: SProxy "image.png")

  test "parseRelFile - ./image.png"
    (parseRelFile posixParser "./image.png")
    (Just $ file $ reflectNonEmpty $ SProxy :: SProxy "image.png")

  test "parseRelFile - foo/image.png"
    (parseRelFile posixParser "foo/image.png")
    (Just $ dirFoo </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseRelFile - ../foo/image.png"
    (parseRelFile posixParser "../foo/image.png")
    (Just $ currentDir <..> dirFoo </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseAbsFile - /image.png"
    (parseAbsFile posixParser "/image.png")
    (Just $ rootDir </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseAbsFile - /foo/image.png"
    (parseAbsFile posixParser "/foo/image.png")
    (Just $ rootDir </> dirFoo </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseRelDir - empty string"
    (parseRelDir posixParser "")
    Nothing

  test "parseRelDir - ./../"
    (parseRelDir posixParser "./../")
    (Just $ currentDir <..> currentDir)

  test "parseRelDir - foo/"
    (parseRelDir posixParser "foo/")
    (Just dirFoo)

  test "parseRelDir - foo/bar"
    (parseRelDir posixParser "foo/bar/")
    (Just $ dirFoo </> dirBar)

  test "parseRelDir - ./foo/bar"
    (parseRelDir posixParser "./foo/bar/")
    (Just $ dirFoo </> dirBar)

  test "parseAbsDir - /"
    (parseAbsDir posixParser "/")
    (Just $ rootDir)

  test "parseAbsDir - /foo/"
    (parseAbsDir posixParser "/foo/")
    (Just $ rootDir </> dirFoo)

  test "parseAbsDir - /foo/bar"
    (parseAbsDir posixParser "/foo/bar/")
    (Just $ rootDir </> dirFoo </> dirBar)

class IsSymbolNonEmpty sym where
  reflectNonEmpty :: SProxy sym -> NonEmptyString

instance isSymbolNonEmpty :: (Symbol.IsSymbol s, Symbol.Equals s "" Symbol.False) => IsSymbolNonEmpty s where
  reflectNonEmpty _ = asNonEmpty $ Symbol.reflectSymbol (SProxy :: SProxy s)
    where
    asNonEmpty :: String -> NonEmptyString
    asNonEmpty = unsafeCoerce
