module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, info, infoShow)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol) as Symbol
import Type.Data.Boolean (False) as Symbol
import Type.Data.Symbol (class Equals) as Symbol
import Data.Path.Pathy (Abs, Dir, File, Path, Rel, Sandboxed, canonicalize, currentDir, depth, dir, dropExtension, file, parentDir', parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, renameFile', rootDir, sandbox, unsafePrintPath, unsandbox, (<..>), (<.>), (</>))
import Data.String as Str
import Data.String.NonEmpty (NonEmptyString)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Laws.Data as Laws.Data
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

test :: forall a eff. Show a => Eq a => String -> a -> a -> Eff (console :: CONSOLE | eff) Unit
test name actual expected= do
  infoShow $ "Test: " <> name
  if expected == actual then infoShow $ "Passed: " <> (show expected) else infoShow $ "Failed: Expected " <> (show expected) <> " but found " <> (show actual)

test' :: forall a b s eff. String -> Path a b s -> String -> Eff (console :: CONSOLE | eff) Unit
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

dirFoo :: forall s. Path Rel Dir s
dirFoo = dir (reflectNonEmpty $ SProxy :: SProxy "foo")

dirBar :: forall s. Path Rel Dir s
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
  --   (printPath (parentDir' currentDir))
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
    (parentDir' currentDir)
    "./../"

  test' "(</>) - ./../foo/"
    (parentDir' currentDir </> unsandbox (dirFoo))
    "./../foo/"

  test' "parentDir' - ./../foo/../"
    ((parentDir' currentDir </> unsandbox (dirFoo)) </> (parentDir' currentDir))
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
    (canonicalize $ parentDir' $ dirFoo)
    "./"

  test' "canonicalize - 2 down, 2 up"
    (canonicalize (parentDir' (parentDir' (dirFoo </> dirBar))))
    "./"

  test "renameFile - single level deep"
    (renameFile' dropExtension (file (reflectNonEmpty $ SProxy :: SProxy "image.png")))
   
   (Just $ file $ reflectNonEmpty $ SProxy :: SProxy "image")

  test' "sandbox - sandbox absolute dir to one level higher"
    (unsafePartial $ fromJust $ sandbox (rootDir </> dirFoo) (rootDir </> dirFoo </> dirBar))
    "./bar/"

  test "depth - negative"
    (depth (parentDir' $ parentDir' $ parentDir' $ currentDir)) (-3)

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
    -- (Just $ currentDir)
    Nothing

  test "parseRelDir - ./../"
    (parseRelDir "./../")
    (Just $ currentDir <..> currentDir)

  test "parseRelDir - foo/"
    (parseRelDir "foo/")
    (Just $ dirFoo)

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
