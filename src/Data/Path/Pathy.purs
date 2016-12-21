module Data.Path.Pathy
  ( Abs
  , AbsDir
  , AbsFile
  , AbsPath
  , Dir
  , DirName(..)
  , Escaper(..)
  , File
  , FileName(..)
  , Path
  , AnyPath
  , Rel
  , RelDir
  , RelFile
  , RelPath
  , Sandboxed
  , Unsandboxed
  , appendPath
  , (</>)
  , setExtension
  , (<.>)
  , parentAppend
  , (<..>)
  , runDirName
  , runFileName
  , canonicalize
  , changeExtension
  , currentDir
  , depth
  , dir
  , dir'
  , dirName
  , dropExtension
  , extension
  , file
  , file'
  , fileName
  , pathName
  , identicalPath
  , isAbsolute
  , isRelative
  , maybeAbs
  , maybeDir
  , maybeFile
  , maybeRel
  , parentDir
  , parentDir'
  , peel
  , posixEscaper
  , parsePath
  , parseAbsDir
  , parseAbsFile
  , parseRelDir
  , parseRelFile
  , printPath
  , printPath'
  , refine
  , relativeTo
  , renameDir
  , renameFile
  , rootDir
  , runEscaper
  , sandbox
  , unsandbox
  , unsafePrintPath
  , unsafePrintPath'
  )
  where

import Prelude

import Data.Array ((!!), filter, length, zipWith, range)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Data.Tuple (Tuple(..), fst, snd)

import Unsafe.Coerce (unsafeCoerce)

-- | The (phantom) type of relative paths.
foreign import data Rel :: *

-- | The (phantom) type of absolute paths.
foreign import data Abs :: *

-- | The (phantom) type of files.
foreign import data File :: *

-- | The (phantom) type of directories.
foreign import data Dir :: *

-- | The (phantom) type of unsandboxed paths.
foreign import data Unsandboxed :: *

-- | The (phantom) type of sandboxed paths.
foreign import data Sandboxed :: *

-- | A newtype around a file name.
newtype FileName = FileName String

-- | Unwraps the `FileName` newtype.
runFileName :: FileName -> String
runFileName (FileName name) = name

-- | A newtype around a directory name.
newtype DirName = DirName String

-- | Unwraps the `DirName` newtype.
runDirName :: DirName -> String
runDirName (DirName name) = name

-- | A type that describes a Path. All flavors of paths are described by this
-- | type, whether they are absolute or relative paths, whether they
-- | refer to files or directories, whether they are sandboxed or not.
-- |
-- | * The type parameter `a` describes whether the path is `Rel` or `Abs`.
-- | * The type parameter `b` describes whether the path is `File` or `Dir`.
-- | * The type parameter `s` describes whether the path is `Sandboxed` or `Unsandboxed`.
-- |
-- | To ensure type safety, there is no way for users to create a value of
-- | this type directly. Instead, helpers should be used, such as `rootDir`,
-- | `currentDir`, `file`, `dir`,  `(</>)`, and `parsePath`.
-- |
-- | This ADT allows invalid paths (e.g. paths inside files), but there is no
-- | possible way for such paths to be constructed by user-land code. The only
-- | "invalid path" that may be constructed is using the `parentDir'` function, e.g.
-- | `parentDir' rootDir`, or by parsing an equivalent string such as `/../`,
-- | but such paths are marked as unsandboxed, and may not be rendered to strings
-- | until they are first sandboxed to some directory.
data Path a b s
  = Current
  | Root
  | ParentIn (Path a b s)
  | DirIn (Path a b s) DirName
  | FileIn (Path a b s) FileName

-- | A type describing a file whose location is given relative to some other,
-- | unspecified directory (referred to as the "current directory").
type RelFile s = Path Rel File s

-- | A type describing a file whose location is absolutely specified.
type AbsFile s = Path Abs File s

-- | A type describing a directory whose location is given relative to some
-- | other, unspecified directory (referred to as the "current directory").
type RelDir s = Path Rel Dir s

-- | A type describing a directory whose location is absolutely specified.
type AbsDir s = Path Abs Dir s

-- | A type describing a file or directory path.
type AnyPath b s = Either (Path b Dir s) (Path b File s)

-- | A type describing a relative file or directory path.
type RelPath s = AnyPath Rel s

-- | A type describing an absolute file or directory path.
type AbsPath s = AnyPath Abs s

-- | Escapers encode segments or characters which have reserved meaning.
newtype Escaper = Escaper (String -> String)

-- | Given an escaper and a segment to encode, returns the encoded segment.
runEscaper :: Escaper -> String -> String
runEscaper (Escaper f) = f

-- | An escaper that does nothing except remove slashes (the bare minimum of
-- | what must be done).
nonEscaper :: Escaper
nonEscaper = Escaper \s -> S.joinWith "" $ filter (_ /= "/") (S.split (S.Pattern "") s)

-- | An escaper that removes all slashes, converts ".." into "$dot$dot", and
-- | converts "." into "$dot".
posixEscaper :: Escaper
posixEscaper = Escaper $
  runEscaper nonEscaper >>>
    case _ of
      ".." -> "$dot$dot"
      "." -> "$dot"
      s -> s

-- | Creates a path which points to a relative file of the specified name.
file :: forall s. String -> Path Rel File s
file f = file' (FileName f)

-- | Creates a path which points to a relative file of the specified name.
file' :: forall s. FileName -> Path Rel File s
file' f = FileIn Current f

-- | Retrieves the name of a file path.
fileName :: forall a s. Path a File s -> FileName
fileName (FileIn _ f) = f
fileName _ = FileName ""

-- | Retrieves the extension of a file name.
extension :: FileName -> String
extension (FileName f) = case S.lastIndexOf (S.Pattern ".") f of
  Just x -> S.drop (x + 1) f
  Nothing -> ""

-- | Drops the extension on a file name.
dropExtension :: FileName -> FileName
dropExtension (FileName n) = case S.lastIndexOf (S.Pattern ".") n of
  Just x -> FileName $ S.take x n
  Nothing -> FileName n

-- | Changes the extension on a file name.
changeExtension :: (String -> String) -> FileName -> FileName
changeExtension f nm =
  update (f $ extension nm) (dropExtension nm)
  where
  update "" n = n
  update ext (FileName n) = FileName $ n <> "." <> ext

-- | Creates a path which points to a relative directory of the specified name.
dir :: forall s. String -> Path Rel Dir s
dir d = dir' (DirName d)

-- | Creates a path which points to a relative directory of the specified name.
dir' :: forall s. DirName -> Path Rel Dir s
dir' d = DirIn Current d

-- | Retrieves the name of a directory path. Not all paths have such a name,
-- | for example, the root or current directory.
dirName :: forall a s. Path a Dir s -> Maybe DirName
dirName p = case canonicalize p of
  DirIn _ d -> Just d
  _ -> Nothing

pathName :: forall b s. AnyPath b s -> Either (Maybe DirName) FileName
pathName = bimap dirName fileName

-- | Given a directory path, appends either a file or directory to the path.
appendPath :: forall a b s. Path a Dir s -> Path Rel b s -> Path a b s
appendPath Current Current = Current
appendPath Root Current = Root
appendPath (ParentIn p1) Current = ParentIn (p1 </> Current)
appendPath (FileIn p1 f1) Current = FileIn (p1 </> Current) f1
appendPath (DirIn p1 d1) Current = DirIn (p1 </> Current) d1
appendPath p1 (ParentIn p2) = ParentIn (p1 </> p2)
appendPath p1 (FileIn p2 f2) = FileIn (p1 </> p2) f2
appendPath p1 (DirIn p2 d2) = DirIn (p1 </> p2) d2
-- following cases don't make sense but cannot exist
appendPath Current Root = Current
appendPath Root Root = Root
appendPath (ParentIn p1) Root = ParentIn (p1 </> Current)
appendPath (FileIn p1 f1) Root = FileIn (p1 </> Current) f1
appendPath (DirIn p1 d1) Root = DirIn (p1 </> Current) d1

infixl 6 appendPath as </>

-- | Sets the extension of the file to the specified extension.
-- |
-- | ```purescript
-- | file "image" <.> "png"
-- | ```
setExtension :: forall a s. Path a File s -> String -> Path a File s
setExtension p ext = renameFile (changeExtension $ const ext) p

infixl 6 setExtension as <.>

-- | Ascends into the parent of the specified directory, then descends into
-- | the specified path. The result is always unsandboxed because it may escape
-- | its previous sandbox.
parentAppend
  :: forall a b s s'
   . Path a Dir s
  -> Path Rel b s'
  -> Path a b Unsandboxed
parentAppend d p = parentDir' d </> unsandbox p

infixl 6 parentAppend as <..>

-- | Determines if this path is absolutely located.
isAbsolute :: forall a b s. Path a b s -> Boolean
isAbsolute Current = false
isAbsolute Root = true
isAbsolute (ParentIn p) = isAbsolute p
isAbsolute (FileIn p _) = isAbsolute p
isAbsolute (DirIn p _) = isAbsolute p

-- | Determines if this path is relatively located.
isRelative :: forall a b s. Path a b s -> Boolean
isRelative = not <<< isAbsolute

-- | Peels off the last directory and the terminal file or directory name
-- | from the path. Returns `Nothing` if there is no such pair (for example,
-- | if the last path segment is root directory, current directory, or parent
-- | directory).
peel
  :: forall a b s
   . Path a b s
  -> Maybe (Tuple (Path a Dir s) (Either DirName FileName))
peel Current = Nothing
peel Root = Nothing
peel p@(ParentIn _) = case canonicalize' p of
  Tuple true p' -> peel p'
  _ -> Nothing
peel (DirIn p d) = Just $ Tuple (unsafeCoerceType p) (Left d)
peel (FileIn p f) = Just $ Tuple (unsafeCoerceType p) (Right f)

-- | Determines if the path refers to a directory.
maybeDir :: forall a b s. Path a b s -> Maybe (Path a Dir s)
maybeDir Current = Just Current
maybeDir Root = Just Root
maybeDir (ParentIn p) = Just $ ParentIn (unsafeCoerceType p)
maybeDir (FileIn _ _) = Nothing
maybeDir (DirIn p d) = Just $ DirIn (unsafeCoerceType p) d

-- | Determines if the path refers to a file.
maybeFile :: forall a b s. Path a b s -> Maybe (Path a File s)
maybeFile Current = Nothing
maybeFile Root = Nothing
maybeFile (ParentIn _) = Nothing
maybeFile (FileIn p f) = (</>) <$> maybeDir p <*> Just (file' f)
maybeFile (DirIn _ _) = Nothing

-- | Determines if the path is relatively specified.
maybeRel :: forall a b s. Path a b s -> Maybe (Path Rel b s)
maybeRel Current = Just Current
maybeRel Root = Nothing
maybeRel (ParentIn p) = ParentIn <$> maybeRel p
maybeRel (FileIn p f) = flip FileIn f <$> maybeRel p
maybeRel (DirIn p d) = flip DirIn  d <$> maybeRel p

-- | Determines if the path is absolutely specified.
maybeAbs :: forall a b s. Path a b s -> Maybe (Path Rel b s)
maybeAbs Current = Nothing
maybeAbs Root = Just Root
maybeAbs (ParentIn p) = ParentIn <$> maybeAbs p
maybeAbs (FileIn p f) = flip FileIn f <$> maybeAbs p
maybeAbs (DirIn p d) = flip DirIn  d <$> maybeAbs p

-- | Returns the depth of the path. This may be negative in some cases, e.g.
-- | `./../../../` has depth `-3`.
depth :: forall a b s. Path a b s -> Int
depth Current = 0
depth Root = 0
depth (ParentIn p) = depth p - 1
depth (FileIn p _) = depth p + 1
depth (DirIn p _) = depth p + 1

-- | Attempts to extract out the parent directory of the specified path. If the
-- | function would have to use a relative path in the return value, the function will
-- | instead return `Nothing`.
parentDir :: forall a b s. Path a b s -> Maybe (Path a Dir s)
parentDir p = fst <$> peel p

-- | Unsandboxes any path (whether sandboxed or not).
unsandbox :: forall a b s. Path a b s -> Path a b Unsandboxed
unsandbox Current = Current
unsandbox Root = Root
unsandbox (ParentIn p) = ParentIn (unsandbox p)
unsandbox (DirIn p d) = DirIn (unsandbox p) d
unsandbox (FileIn p f) = FileIn (unsandbox p) f

-- | Creates a path that points to the parent directory of the specified path.
-- | This function always unsandboxes the path.
parentDir' :: forall a b s. Path a b s -> Path a Dir Unsandboxed
parentDir' = ParentIn <<< unsafeCoerceType <<< unsandbox

unsafeCoerceType :: forall a b b' s. Path a b s -> Path a b' s
unsafeCoerceType = unsafeCoerce

  -- | The "current directory", which can be used to define relatively-located resources.
currentDir :: forall s. Path Rel Dir s
currentDir = Current

-- | The root directory, which can be used to define absolutely-located resources.
rootDir :: forall s. Path Abs Dir s
rootDir = Root

-- | Renames a file path.
renameFile :: forall a s. (FileName -> FileName) -> Path a File s -> Path a File s
renameFile f (FileIn p f0) = FileIn p (f f0)
renameFile _ p = p

-- | Renames a directory path. Note: This is a simple rename of the terminal
-- | directory name, not a "move".
renameDir :: forall a s. (DirName -> DirName) -> Path a Dir s -> Path a Dir s
renameDir f (DirIn p d) = DirIn p (f d)
renameDir _ p = p

-- | Canonicalizes a path, by reducing things in the form `/x/../` to just `/x/`.
canonicalize :: forall a b s. Path a b s -> Path a b s
canonicalize = snd <<< canonicalize'

-- | Canonicalizes a path and returns information on whether or not it actually changed.
canonicalize' :: forall a b s. Path a b s -> Tuple Boolean (Path a b s)
canonicalize' Current = Tuple false Current
canonicalize' Root = Tuple false Root
canonicalize' (ParentIn (FileIn p f)) = Tuple true  (snd $ canonicalize' p)
canonicalize' (ParentIn (DirIn  p f)) = Tuple true  (snd $ canonicalize' p)
canonicalize' (ParentIn p) = case canonicalize' p of
  Tuple changed p' ->
    let p'' = ParentIn p'
    in if changed then canonicalize' p'' else Tuple changed p''
canonicalize' (FileIn p f) = flip FileIn f <$> canonicalize' p
canonicalize' (DirIn p d) = flip DirIn d <$> canonicalize' p

unsafePrintPath' :: forall a b s. Escaper -> Path a b s -> String
unsafePrintPath' r = go
  where
    go Current = "./"
    go Root = "/"
    go (ParentIn p) = go p <> "../"
    go (DirIn p@(FileIn _ _ ) (DirName d)) = go p <> "/" <> escape d <> "/" -- dir inside a file
    go (DirIn p (DirName d)) = go p <> escape d <> "/" -- dir inside a dir
    go (FileIn p@(FileIn  _ _) (FileName f)) = go p <> "/" <> escape f -- file inside a file
    go (FileIn p (FileName f)) = go p <> escape f
    escape = runEscaper r

unsafePrintPath :: forall a b s. Path a b s -> String
unsafePrintPath = unsafePrintPath' posixEscaper

-- | Prints a `Path` into its canonical `String` representation. For security
-- | reasons, the path must be sandboxed before it can be rendered to a string.
printPath :: forall a b. Path a b Sandboxed -> String
printPath = unsafePrintPath

-- | Prints a `Path` into its canonical `String` representation, using the
-- | specified escaper to escape special characters in path segments. For
-- | security reasons, the path must be sandboxed before rendering to string.
printPath' :: forall a b. Escaper -> Path a b Sandboxed -> String
printPath' = unsafePrintPath'

-- | Determines if two paths have the exact same representation. Note that
-- | two paths may represent the same path even if they have different
-- | representations!
identicalPath :: forall a a' b b' s s'. Path a b s -> Path a' b' s' -> Boolean
identicalPath p1 p2 = show p1 == show p2

-- | Makes one path relative to another reference path, if possible, otherwise
-- | returns `Nothing`. The returned path inherits the sandbox settings of the
-- | reference path.
-- |
-- | Note there are some cases this function cannot handle.
relativeTo :: forall a b s s'. Path a b s -> Path a Dir s' -> Maybe (Path Rel b s')
relativeTo p1 p2 = relativeTo' (canonicalize p1) (canonicalize p2)
  where
  relativeTo' :: forall b'. Path a b' s -> Path a Dir s' -> Maybe (Path Rel b' s')
  relativeTo' Root Root = Just Current
  relativeTo' Current Current = Just Current
  relativeTo' cp1 cp2
    | identicalPath cp1 cp2 = Just Current
    | otherwise = case peel cp1 of
        Just (Tuple cp1' e) ->
          flip (</>) (either (DirIn Current) (FileIn Current) e) <$> relativeTo' cp1' cp2
        Nothing -> Nothing

-- | Attempts to sandbox a path relative to some directory. If successful, the sandboxed
-- | directory will be returned relative to the sandbox directory (although this can easily
-- | be converted into an absolute path using `</>`).
-- |
-- | This combinator can be used to ensure that paths which originate from user-code
-- | cannot access data outside a given directory.
sandbox :: forall a b s. Path a Dir Sandboxed -> Path a b s -> Maybe (Path Rel b Sandboxed)
sandbox p1 p2 = p2 `relativeTo` p1

-- | Refines path segments but does not change anything else.
refine :: forall a b s. (FileName -> FileName) -> (DirName -> DirName) -> Path a b s -> Path a b s
refine f d = go
  where go (Current      ) = Current
        go (Root         ) = Root
        go (ParentIn p   ) = ParentIn (go p)
        go (DirIn    p d0) = DirIn    (go p) (d d0)
        go (FileIn   p f0) = FileIn   (go p) (f f0)

-- | Parses a canonical `String` representation of a path into a `Path` value.
-- | Note that in order to be unambiguous, trailing directories should be
-- | marked with a trailing slash character (`'/'`).
parsePath
  :: forall z
   . (RelDir Unsandboxed -> z)
  -> (AbsDir Unsandboxed -> z)
  -> (RelFile Unsandboxed -> z)
  -> (AbsFile Unsandboxed -> z)
  -> String
  -> z
parsePath rd ad rf af "" = rd Current
parsePath rd ad rf af p =
  let
    segs    = S.split (S.Pattern "/") p
    last    = length segs - 1
    isAbs   = S.take 1 p == "/"
    isFile  = maybe false (_ /= "") (segs !! last)
    tuples  = zipWith Tuple segs (range 0 last)

    folder :: forall a b s. Path a b s -> Tuple String Int -> Path a b s
    folder base (Tuple seg idx) =
      case seg of
        "." -> base
        "" -> base
        ".." -> ParentIn base
        _ | isFile && idx == last -> FileIn base (FileName seg)
          | otherwise -> DirIn base (DirName seg)
  in
    case isAbs, isFile of
      true, true -> af (foldl folder Root tuples)
      true, false -> ad (foldl folder Root tuples)
      false, true -> rf (foldl folder Current tuples)
      false, false -> rd (foldl folder Current tuples)

-- | Attempts to parse a relative file from a string.
parseRelFile :: String -> Maybe (RelFile Unsandboxed)
parseRelFile = parsePath (const Nothing) (const Nothing) Just (const Nothing)

-- | Attempts to parse an absolute file from a string.
parseAbsFile :: String -> Maybe (AbsFile Unsandboxed)
parseAbsFile = parsePath (const Nothing) (const Nothing) (const Nothing) Just

-- | Attempts to parse a relative directory from a string.
parseRelDir :: String -> Maybe (RelDir Unsandboxed)
parseRelDir = parsePath Just (const Nothing) (const Nothing) (const Nothing)

-- | Attempts to parse an absolute directory from a string.
parseAbsDir :: String -> Maybe (AbsDir Unsandboxed)
parseAbsDir = parsePath (const Nothing) Just (const Nothing) (const Nothing)

instance showPath :: Show (Path a b s) where
  show Current = "currentDir"
  show Root = "rootDir"
  show (ParentIn p) = "(parentDir' " <> show p <> ")"
  show (FileIn p (FileName f)) = "(" <> show p <> " </> file " <> show f <> ")"
  show (DirIn p (DirName f)) = "(" <> show p <> " </> dir "  <> show f <> ")"

instance eqPath :: Eq (Path a b s) where
  eq p1 p2 = canonicalize p1 `identicalPath` canonicalize p2

instance ordPath :: Ord (Path a b s) where
  compare p1 p2 = go (canonicalize p1) (canonicalize p2)
    where
    go Current Current = EQ
    go Current _ = LT
    go _ Current = GT
    go Root Root = EQ
    go Root _ = LT
    go _ Root = GT
    go (ParentIn p1') (ParentIn p2') = compare p1' p2'
    go (ParentIn _) _ = LT
    go _ (ParentIn _) = GT
    go (DirIn p1' d1) (DirIn p2' d2) = compare p1' p2' <> compare d1 d2
    go (DirIn _ _) _ = LT
    go _ (DirIn _ _) = GT
    go (FileIn p1' f1) (FileIn p2' f2) = compare p1' p2' <> compare f1 f2

derive instance genericPath :: Generic (Path a b s)

instance showFileName :: Show FileName where
  show (FileName name) = "(FileName " <> show name <> ")"

derive instance eqFileName :: Eq FileName
derive instance ordFileName :: Ord FileName
derive instance genericFileName :: Generic FileName

instance showDirName :: Show DirName where
  show (DirName name) = "(DirName " <> show name <> ")"

derive instance eqDirName :: Eq DirName
derive instance ordDirName :: Ord DirName
derive instance genericDirName :: Generic DirName
