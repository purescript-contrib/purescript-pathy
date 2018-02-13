module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, info, infoShow)
import Data.Either (either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromJust)
import Data.Path.Pathy (class SplitDirOrFile, class SplitRelOrAbs, Abs, Dir, File, Path, Rel, Sandboxed, canonicalize, currentDir, depth, dir, dirOrFile, dropExtension, file, parentDir, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, relOrAbs, renameFile', rootDir, sandbox, unsafePrintPath, unsandbox, (<..>), (<.>), (</>))
import Data.String as Str
import Data.String.NonEmpty (NonEmptyString)
import Data.Symbol (SProxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol) as Symbol
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Laws.Data as Laws.Data
import Type.Data.Boolean (False) as Symbol
import Type.Data.Symbol (class Equals) as Symbol
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

test :: forall a eff. Show a => Eq a => String -> a -> a -> Eff (console :: CONSOLE | eff) Unit
test name actual expected= do
  infoShow $ "Test: " <> name
  if expected == actual then infoShow $ "Passed: " <> (show expected) else infoShow $ "Failed: Expected " <> (show expected) <> " but found " <> (show actual)

test' :: forall a b s eff. SplitDirOrFile b => String -> Path a b s -> String -> Eff (console :: CONSOLE | eff) Unit
test' n p s = test n (unsafePrintPath p) s

newtype ArbPath = ArbPath (Path Abs File Sandboxed)

derive newtype instance eqArbPath :: Eq ArbPath
derive newtype instance ordArbPath :: Ord ArbPath

runArbPath ∷ ArbPath → (Path Abs File Sandboxed)
runArbPath (ArbPath p) = p

instance arbitraryArbPath ∷ QC.Arbitrary ArbPath where
  arbitrary = do
    numDirs ← Gen.chooseInt 1 10
    dirs ← map dir <$> Gen.vectorOf numDirs pathPart
    filename ← file <$> pathPart
    pure $ ArbPath $ rootDir </> foldl (flip (</>)) filename (dirs ∷ Array (Path Rel Dir Sandboxed))

pathPart ∷ Gen.Gen NonEmptyString
pathPart = asNonEmptyString <$> Gen.suchThat QC.arbitrary (not <<< Str.null)
  where
  asNonEmptyString :: String -> NonEmptyString
  asNonEmptyString = unsafeCoerce

dirFoo :: Path Rel Dir Sandboxed
dirFoo = dir (reflectNonEmpty $ SProxy :: SProxy "foo")

dirBar :: Path Rel Dir Sandboxed
dirBar = dir (reflectNonEmpty $ SProxy :: SProxy "bar")

main :: QC.QC () Unit
main = do
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

  -- Should not compile:
  -- test
  --   "printPath -- cannot print unsandboxed"
  --   (printPath (parentDir currentDir))
  --   "./../"

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
    (parentDir currentDir)
    "./../"

  test' "(</>) - ./../foo/"
    (parentDir currentDir </> unsandbox (dirFoo))
    "./../foo/"

  test' "parentDir - ./../foo/../"
    ((parentDir currentDir </> unsandbox (dirFoo)) </> (parentDir currentDir))
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
    (canonicalize $ parentDir $ dirFoo)
    "./"

  test' "canonicalize - 2 down, 2 up"
    (canonicalize (parentDir (parentDir (dirFoo </> dirBar))))
    "./"

  test "renameFile - single level deep"
    (renameFile' dropExtension (file (reflectNonEmpty $ SProxy :: SProxy "image.png")))

   (Just $ file $ reflectNonEmpty $ SProxy :: SProxy "image")

  test' "sandbox - sandbox absolute dir to one level higher"
    (unsafePartial $ fromJust $ sandbox (rootDir </> dirFoo) (rootDir </> dirFoo </> dirBar))
    "./bar/"

  test "depth - negative"
    (depth (parentDir $ parentDir $ parentDir $ currentDir)) (-3)

  test "parseRelFile - image.png"
    (parseRelFile "image.png")
    (Just $ unsandbox $ file $ reflectNonEmpty $ SProxy :: SProxy "image.png")

  test "parseRelFile - ./image.png"
    (parseRelFile "./image.png")
    (Just $ unsandbox $ file $ reflectNonEmpty $ SProxy :: SProxy "image.png")

  test "parseRelFile - foo/image.png"
    (parseRelFile "foo/image.png")
    (Just $ unsandbox $ dirFoo </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseRelFile - ../foo/image.png"
    (parseRelFile "../foo/image.png")
    (Just $ unsandbox $ currentDir <..> dirFoo </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseAbsFile - /image.png"
    (parseAbsFile "/image.png")
    (Just $ unsandbox $ rootDir </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseAbsFile - /foo/image.png"
    (parseAbsFile "/foo/image.png")
    (Just $ unsandbox $ rootDir </> dirFoo </> file (reflectNonEmpty $ SProxy :: SProxy "image.png"))

  test "parseRelDir - empty string"
    (parseRelDir "")
    -- (Just $ currentDir)
    Nothing

  test "parseRelDir - ./../"
    (parseRelDir "./../")
    (Just $ currentDir <..> currentDir)

  test "parseRelDir - foo/"
    (parseRelDir "foo/")
    (Just $ unsandbox dirFoo)

  test "parseRelDir - foo/bar"
    (parseRelDir "foo/bar/")
    (Just $ unsandbox $ dirFoo </> dirBar)

  test "parseRelDir - ./foo/bar"
    (parseRelDir "./foo/bar/")
    (Just $ unsandbox $ dirFoo </> dirBar)

  test "parseAbsDir - /"
    (parseAbsDir "/")
    (Just $ unsandbox rootDir)

  test "parseAbsDir - /foo/"
    (parseAbsDir "/foo/")
    (Just $ unsandbox $ rootDir </> dirFoo)

  test "parseAbsDir - /foo/bar"
    (parseAbsDir "/foo/bar/")
    (Just $ unsandbox $ rootDir </> dirFoo </> dirBar)

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


-- | Determines if the path refers to a directory.
maybeDir :: forall a b s. SplitDirOrFile b => Path a b s -> Maybe (Path a Dir s)
maybeDir p = either Just (const Nothing) (dirOrFile p)

-- | Determines if the path refers to a file.
maybeFile :: forall a b s. SplitDirOrFile b => Path a b s -> Maybe (Path a File s)
maybeFile p = either (const Nothing) Just (dirOrFile p)

-- | Determines if the path is relatively specified.
maybeRel :: forall a b s. SplitRelOrAbs a => Path a b s -> Maybe (Path Rel b s)
maybeRel p = either Just (const Nothing) (relOrAbs p)

-- | Determines if the path is absolutely specified.
maybeAbs :: forall a b s. SplitRelOrAbs a => Path a b s -> Maybe (Path Abs b s)
maybeAbs p = either (const Nothing) Just (relOrAbs p)

-- | Determines if this path is absolutely located.
isAbsolute :: forall a b s. SplitRelOrAbs a => Path a b s -> Boolean
isAbsolute p = either (const false) (const true) (relOrAbs p)

-- | Determines if this path is relatively located.
isRelative :: forall a b s. SplitRelOrAbs a => Path a b s -> Boolean
isRelative p= either (const true) (const false) (relOrAbs p)
