module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Array.NonEmpty (cons')
import Data.String as Str
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty (fromString) as NES
import Data.String.NonEmpty.CodeUnits (singleton) as NES
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (info)
import Effect.Exception (throw)
import Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, Name(..), Path, Rel, alterExtension, currentDir, debugPrintPath, dir, extension, file, in', joinName, parentOf, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, peel, posixParser, posixPrinter, printPath, relativeTo, rename, rootDir, sandbox, sandboxAny, splitName, unsandbox, windowsPrinter, (<..>), (<.>), (</>))
import Pathy.Gen as PG
import Pathy.Name (reflectName)
import Type.Proxy (Proxy(..))
import Test.QuickCheck ((===))
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as Gen
import Unsafe.Coerce (unsafeCoerce)

test :: forall a. Show a => Eq a => String -> a -> a -> Effect Unit
test name actual expected = do
  info $ "Test: " <> name
  if expected == actual then info $ "Passed: " <> (show expected)
  else throw $ "Failed:\n    Expected: " <> (show expected) <> "\n    Actual:   " <> (show actual)

test' :: forall a b. IsRelOrAbs a => IsDirOrFile b => String -> Path a b -> String -> Effect Unit
test' n p s = test n (printTestPath p) s

pathPart :: Gen.Gen NonEmptyString
pathPart = asNonEmptyString <$> Gen.suchThat QC.arbitrary (not <<< Str.null)
  where
  asNonEmptyString :: String -> NonEmptyString
  asNonEmptyString = unsafeCoerce

dirFoo :: Path Rel Dir
dirFoo = dir (Proxy :: Proxy "foo")

dirBar :: Path Rel Dir
dirBar = dir (Proxy :: Proxy "bar")

dirBaz :: Path Rel Dir
dirBaz = dir (Proxy :: Proxy "baz")

parsePrintCheck :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> Maybe (Path a b) -> QC.Result
parsePrintCheck input parsed =
  if parsed == Just input then QC.Success
  else QC.Failed
    $ "`parse (print path) != Just path` for path: `" <> show input <> "` which was re-parsed into `" <> show parsed <> "`"
        <> "\n\tPrinted path: "
        <> show (printTestPath input)
        <> "\n\tPrinted path': `"
        <> show (map (printTestPath) parsed)
        <> "`"

parsePrintAbsDirPath :: Gen.Gen QC.Result
parsePrintAbsDirPath = PG.genAbsDirPath <#> \path ->
  parsePrintCheck path (parseAbsDir posixParser $ printTestPath path)

parsePrintAbsFilePath :: Gen.Gen QC.Result
parsePrintAbsFilePath = PG.genAbsFilePath <#> \path ->
  parsePrintCheck path (parseAbsFile posixParser $ printTestPath path)

parsePrintRelDirPath :: Gen.Gen QC.Result
parsePrintRelDirPath = PG.genRelDirPath <#> \path ->
  parsePrintCheck path (parseRelDir posixParser $ printTestPath path)

parsePrintRelFilePath :: Gen.Gen QC.Result
parsePrintRelFilePath = PG.genRelFilePath <#> \path ->
  parsePrintCheck path (parseRelFile posixParser $ printTestPath path)

genAmbigiousName :: forall a. Gen.Gen (Name a)
genAmbigiousName =
  let
    genNES = PG.genName <#> un Name
  in
    map Name $ Gen.oneOf $ cons' genNES
      [ genNES <#> \a -> a <> (NES.singleton '.')
      , genNES <#> \a -> (NES.singleton '.') <> a
      , pure (NES.singleton '.')
      , do
          a <- genNES
          b <- genNES
          pure $ a <> (NES.singleton '.') <> b
      ]

checkAlterExtensionId :: Gen.Gen QC.Result
checkAlterExtensionId = do
  n <- genAmbigiousName
  pure $ alterExtension identity n === identity n

checkJoinSplitNameId :: Gen.Gen QC.Result
checkJoinSplitNameId = do
  n <- genAmbigiousName
  pure $ joinName (splitName n) === identity n

checkPeelIn :: forall b. IsDirOrFile b => Gen.Gen (Path Abs b) -> Gen.Gen QC.Result
checkPeelIn gen = do
  p <- gen
  pure $ p === maybe p (\(Tuple r n) -> r </> in' n) (peel p)

checkRelative :: forall b. IsDirOrFile b => Gen.Gen (Path Abs b) -> Gen.Gen QC.Result
checkRelative gen = do
  p1 <- gen
  p2 <- PG.genAbsDirPath
  let rel = p1 `relativeTo` p2
  let p1' = p2 </> rel
  pure
    if p1 == p1' then QC.Success
    else
      QC.Failed
        $ "`relativeTo` property did not hold:"
            <> "\n\tp1:  "
            <> printTestPath p1
            <> "\n\tp2:  "
            <> printTestPath p2
            <> "\n\trel:  "
            <> printTestPath rel
            <> "\n\tp1': "
            <> printTestPath p1'

main :: Effect Unit
main = do
  info "checking `parse <<< print` for `AbsDir`" *> QC.quickCheck parsePrintAbsDirPath
  info "checking `parse <<< print` for `AbsFile`" *> QC.quickCheck parsePrintAbsFilePath
  info "checking `parse <<< print` for `RelDir`" *> QC.quickCheck parsePrintRelDirPath
  info "checking `parse <<< print` for `RelFile`" *> QC.quickCheck parsePrintRelFilePath
  info "checking `relativeTo` for `AbsDir`" *> QC.quickCheck (checkRelative PG.genAbsDirPath)
  info "checking `relativeTo` for `AbsFile`" *> QC.quickCheck (checkRelative PG.genAbsFilePath)
  info "checking `p === maybe p (\\(Tuple r n) -> r </> in' n) (peel p)` for `AbsDir`" *> QC.quickCheck (checkPeelIn PG.genAbsDirPath)
  info "checking `p === maybe p (\\(Tuple r n) -> r </> in' n) (peel p)` for `AbsFile`" *> QC.quickCheck (checkPeelIn PG.genAbsFilePath)
  info "checking `joinName <<< splitName === id`" *> QC.quickCheck checkJoinSplitNameId
  info "checking `alterExtension id === id`" *> QC.quickCheck checkAlterExtensionId

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

  test "windowsPrinter"
    (printWindowsPath $ rootDir </> dir (Proxy :: Proxy "C") </> dirBar)
    "C:\\bar\\"

  test' "(</>) - file with two parents"
    ( dirFoo
        </> dirBar
        </> file (Proxy :: Proxy "image.png")
    )
    "./foo/bar/image.png"

  test' "(<.>) - file without extension"
    ( file (Proxy :: Proxy "image")
        <.> "png"
    )
    "./image.png"

  test' "(<.>) - file with extension"
    ( file (Proxy :: Proxy "image.jpg")
        <.> "png"
    )
    "./image.png"

  test' "printPath - ./../"
    (parentOf currentDir)
    "./../"

  test """printPath windowsPrinter - C:\Users\Default\"""
    (printPath windowsPrinter $ sandboxAny $ rootDir </> dir (Proxy :: Proxy "C") </> dir (Proxy :: Proxy "Users") </> dir (Proxy :: Proxy "Default"))
    """C:\Users\Default\"""

  test """printPath posixPrinter - /C/Users/Default/"""
    (printPath posixPrinter $ sandboxAny $ rootDir </> dir (Proxy :: Proxy "C") </> dir (Proxy :: Proxy "Users") </> dir (Proxy :: Proxy "Default"))
    """/C/Users/Default/"""

  test """printPath windowsPrinter - \"""
    (printPath windowsPrinter $ sandboxAny rootDir)
    """\"""

  test """printPath posixPrinter - /"""
    (printPath posixPrinter $ sandboxAny rootDir)
    """/"""

  test' "(</>) - ./../foo/"
    (parentOf currentDir </> dirFoo)
    "./../foo/"

  test' "parentOf - ./../foo/../"
    ((parentOf currentDir </> dirFoo) </> (parentOf currentDir))
    "./../"

  test' "(<..>) - ./../"
    (currentDir <..> currentDir)
    "./../"

  test' "(<..>) - ./../foo/"
    (currentDir <..> dirFoo)
    "./../foo/"

  test' "(<..>) - ./../foo/../"
    ((currentDir <..> dirFoo) <..> currentDir)
    "./../"

  test' "./foo/../ = ./"
    (parentOf dirFoo)
    "./"

  test' "./foo/bar/../../ = ./"
    ((parentOf (parentOf (dirFoo </> dirBar))))
    "./"

  test' "/../../ = /"
    ((parentOf (parentOf rootDir)))
    "/"

  test "/foo/../bar/ = /bar"
    ((rootDir </> dirFoo <..> dirBar))
    (rootDir </> dirBar)

  test "/foo/bar/ </> ../bar/ = /foo/bar/"
    ((rootDir </> dirFoo </> dirBar) </> (currentDir <..> dirBar))
    (rootDir </> dirFoo </> dirBar)

  test "/foo/bar/ </> ../../bar/ = /bar/"
    ((rootDir </> dirFoo </> dirBar) </> (currentDir <..> currentDir <..> currentDir </> dirBar))
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
    (rename (alterExtension (const Nothing)) (file (Proxy :: Proxy "image.png")))
    (file (Proxy :: Proxy "image"))

  test """extension (Name ".foo")    == Nothing"""
    (extension (reflectName (Proxy :: Proxy ".foo")))
    (Nothing)
  test """extension (Name "foo.")    == Nothing"""
    (extension (reflectName (Proxy :: Proxy "foo.")))
    (Nothing)
  test """extension (Name "foo")    == Nothing"""
    (extension (reflectName (Proxy :: Proxy "foo")))
    (Nothing)
  test """extension (Name ".")       == Nothing"""
    (extension (reflectName (Proxy :: Proxy ".")))
    (Nothing)
  test """extension (Name "foo.baz") == (Just "baz")"""
    (extension (reflectName (Proxy :: Proxy "foo.baz")))
    (NES.fromString "baz")

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
    (Just $ file (Proxy :: Proxy "image.png"))

  test "parseRelFile - ./image.png"
    (parseRelFile posixParser "./image.png")
    (Just $ file (Proxy :: Proxy "image.png"))

  test "parseRelFile - foo/image.png"
    (parseRelFile posixParser "foo/image.png")
    (Just $ dirFoo </> file (Proxy :: Proxy "image.png"))

  test "parseRelFile - ../foo/image.png"
    (parseRelFile posixParser "../foo/image.png")
    (Just $ currentDir <..> dirFoo </> file (Proxy :: Proxy "image.png"))

  test "parseAbsFile - /image.png"
    (parseAbsFile posixParser "/image.png")
    (Just $ rootDir </> file (Proxy :: Proxy "image.png"))

  test "parseAbsFile - /foo/image.png"
    (parseAbsFile posixParser "/foo/image.png")
    (Just $ rootDir </> dirFoo </> file (Proxy :: Proxy "image.png"))

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

  test "parseRelDir - /foo/././//bar/"
    (parseAbsDir posixParser "/foo/././//bar/")
    (Just $ rootDir </> dirFoo </> dirBar)

printTestPath :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> String
printTestPath p = debugPrintPath posixPrinter p

printWindowsPath :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> String
printWindowsPath p = debugPrintPath windowsPrinter p
