module Test.Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, infoShow)
import Control.Monad.Eff(Eff)
import Data.Maybe (Maybe(..), fromJust)
import Data.Path.Pathy (Path, dir, rootDir, parseAbsDir, parseRelDir, currentDir, file, parseAbsFile, parseRelFile, parentDir', depth, sandbox, dropExtension, renameFile, canonicalize, unsandbox, unsafePrintPath, (</>), (<..>), (<.>))
import Partial.Unsafe (unsafePartial)

test :: forall a. (Show a, Eq a) => String -> a -> a -> Eff (console :: CONSOLE) Unit
test name actual expected= do
  infoShow $ "Test: " <> name
  if expected == actual then infoShow $ "Passed: " <> (show expected) else infoShow $ "Failed: Expected " <> (show expected) <> " but found " <> (show actual)

test' :: forall a b s. String -> Path a b s -> String -> Eff (console :: CONSOLE) Unit
test' n p s = test n (unsafePrintPath p) s

main :: Eff (console :: CONSOLE) Unit
main = do
  infoShow "NEW TEST"

  -- Should not compile:
  -- test "(</>) - file in dir" (printPath (file "image.png" </> dir "foo")) "./image.png/foo"

  -- Should not compile:
  -- test "(</>) - absolute dir in absolute dir" (printPath (rootDir </> rootDir)) "/"

  -- Should not compile:
  -- test "(</>) - absolute dir in relative dir" (printPath (currentDir </> rootDir)) "/"

  -- Should not compile:
  -- test "printPath -- cannot print unsandboxed" (printPath (parentDir' currentDir)) "./../"

  test' "(</>) - two directories"  (dir "foo" </> dir "bar") "./foo/bar/"

  test' "(</>) - file with two parents" (dir "foo" </> dir "bar" </> file "image.png") "./foo/bar/image.png"

  test' "(<.>) - file without extension" (file "image" <.> "png") "./image.png"

  test' "(<.>) - file with extension" (file "image.jpg" <.> "png") "./image.png"

  test' "printPath - ./../" (parentDir' currentDir) "./../"

  test' "(</>) - ./../foo/" (parentDir' currentDir </> unsandbox (dir "foo")) "./../foo/"

  test' "parentDir' - ./../foo/../" ((parentDir' currentDir </> unsandbox (dir "foo")) </> (parentDir' currentDir)) "./../foo/../"

  test' "(<..>) - ./../" (currentDir <..> currentDir) "./../"

  test' "(<..>) - ./../foo/" (currentDir <..> dir "foo") "./../foo/"

  test' "(<..>) - ./../foo/../" ((currentDir <..> dir "foo") <..> currentDir) "./../foo/../"

  test' "canonicalize - 1 down, 1 up" (canonicalize $ parentDir' $ dir "foo") "./"

  test' "canonicalize - 2 down, 2 up" (canonicalize (parentDir' (parentDir' (dir "foo" </> dir "bar")))) "./"

  test' "renameFile - single level deep" (renameFile dropExtension (file "image.png")) "./image"

  test' "sandbox - sandbox absolute dir to one level higher"
        (unsafePartial $ fromJust $ sandbox (rootDir </> dir "foo") (rootDir </> dir "foo" </> dir "bar")) "./bar/"

  test "depth - negative" (depth (parentDir' $ parentDir' $ parentDir' $ currentDir)) (-3)

  test "parseRelFile - image.png" (parseRelFile "image.png") (Just $ file "image.png")

  test "parseRelFile - ./image.png" (parseRelFile "./image.png") (Just $ file "image.png")

  test "parseRelFile - foo/image.png" (parseRelFile "foo/image.png") (Just $ dir "foo" </> file "image.png")

  test "parseRelFile - ../foo/image.png" (parseRelFile "../foo/image.png") (Just $ currentDir <..> dir "foo" </> file "image.png")

  test "parseAbsFile - /image.png" (parseAbsFile "/image.png") (Just $ rootDir </> file "image.png")

  test "parseAbsFile - /foo/image.png" (parseAbsFile "/foo/image.png") (Just $ rootDir </> dir "foo" </> file "image.png")

  test "parseRelDir - empty string" (parseRelDir "") (Just $ currentDir)

  test "parseRelDir - ./../" (parseRelDir "./../") (Just $ currentDir <..> currentDir)

  test "parseRelDir - foo/" (parseRelDir "foo/") (Just $ dir "foo")

  test "parseRelDir - foo/bar" (parseRelDir "foo/bar/") (Just $ dir "foo" </> dir "bar")

  test "parseRelDir - ./foo/bar" (parseRelDir "./foo/bar/") (Just $ dir "foo" </> dir "bar")

  test "parseAbsDir - /" (parseAbsDir "/") (Just $ rootDir)

  test "parseAbsDir - /foo/" (parseAbsDir "/foo/") (Just $ rootDir </> dir "foo")

  test "parseAbsDir - /foo/bar" (parseAbsDir "/foo/bar/") (Just $ rootDir </> dir "foo" </> dir "bar")
