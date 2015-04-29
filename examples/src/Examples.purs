module Examples where 
  import Debug.Trace(Trace(), trace)
  import Control.Monad.Eff(Eff())
  import Data.Maybe(Maybe(..))
  import Data.Maybe.Unsafe(fromJust)
  import Data.Path.Pathy

  test :: forall a. (Show a, Eq a) => String -> a -> a -> Eff (trace :: Trace) Unit
  test name actual expected= do
    trace $ "Test: " ++ name 
    if expected == actual then trace $ "Passed: " ++ (show expected) else trace $ "Failed: Expected " ++ (show expected) ++ " but found " ++ (show actual)

  test' :: forall a b s. String -> Path a b s -> String -> Eff (trace :: Trace) Unit
  test' n p s = test n (unsafePrintPath p) s

  main = do
    trace "NEW TEST"

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
          (fromJust $ sandbox (rootDir </> dir "foo") (rootDir </> dir "foo" </> dir "bar")) "./bar/"

    test "depth - negative" (depth (parentDir' $ parentDir' $ parentDir' $ currentDir)) (-3)

    test "parseRelDir - empty string" (parseRelDir "") (Just $ currentDir)

    test "parseRelFile - image.png" (parseRelFile "image.png") (Just $ file "image.png")
