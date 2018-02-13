module Data.Path.Pathy
  ( Abs
  , AbsDir
  , ParseError
  , AbsFile
  , AbsPath
  , Dir
  , Name(..)
  , Escaper(..)
  , File
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
  , runName
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
  , class SplitRelOrAbs
  , relOrAbs
  , class SplitDirOrFile
  , dirOrFileF
  , dirOrFile
  , dirOrFileName
  , refine
  , relativeTo
  , renameDir
  , renameFile
  , renameFile'
  , rootDir
  , runEscaper
  , sandbox
  , unsandbox
  , unsafePrintPath
  , unsafePrintPath'
  )
  where

import Prelude

import Data.Array (drop, dropEnd, filter, length)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.String as S
import Data.String.NonEmpty (NonEmptyString, appendString)
import Data.String.NonEmpty (fromString, toString) as NEString
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

foreign import kind RelOrAbs

foreign import kind DirOrFile

foreign import kind SandboxedOrNot

-- | The (phantom) type of relative paths.
foreign import data Rel :: RelOrAbs

-- | The (phantom) type of absolute paths.
foreign import data Abs :: RelOrAbs

-- | The (phantom) type of files.
foreign import data File :: DirOrFile

-- | The (phantom) type of directories.
foreign import data Dir :: DirOrFile

-- | The (phantom) type of unsandboxed paths.
foreign import data Unsandboxed :: SandboxedOrNot

-- | The (phantom) type of sandboxed paths.
foreign import data Sandboxed :: SandboxedOrNot

-- | A newtype around a file name.
newtype Name (n :: DirOrFile) = Name NonEmptyString

-- | Unwraps the `Name` newtype.
runName :: forall a. Name a -> String
runName (Name name) = NEString.toString name

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
data Path (a :: RelOrAbs) (b :: DirOrFile) (s :: SandboxedOrNot)
  = Current
  | Root
  | ParentIn (Path a b s)
  | In (Path a Dir s) (Name b)

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

newtype PathFlipped a s b = PathFlipped (Path a b s)
derive instance newtypePathFlipped ∷ Newtype (PathFlipped a s b) _

class SplitDirOrFile (x :: DirOrFile) where
  dirOrFileF :: forall f. f x -> Either (f Dir) (f File)

instance relSplitDirOrFile :: SplitDirOrFile Dir where dirOrFileF = Left
instance absSplitDirOrFile :: SplitDirOrFile File where dirOrFileF = Right

dirOrFile :: forall a b s. SplitDirOrFile b => Path a b s -> AnyPath a s
dirOrFile p = bimap (un PathFlipped) (un PathFlipped) $ dirOrFileF (PathFlipped p)
  
dirOrFileName :: forall b. SplitDirOrFile b => Name b -> Either (Name Dir) (Name File)
dirOrFileName = dirOrFileF

class SplitRelOrAbs (a :: RelOrAbs) where
  relOrAbs :: forall b s. Path a b s -> Either (Path Rel b s) (Path Abs b s)

instance relSplitRelOrAbs :: SplitRelOrAbs Rel where relOrAbs = Left
instance absSplitRelOrAbs :: SplitRelOrAbs Abs where relOrAbs = Right

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
file :: forall s. NonEmptyString -> Path Rel File s
file f = file' (Name f)

-- | Creates a path which points to a relative file of the specified name.
file' :: forall s. Name File -> Path Rel File s
file' f = In Current f

-- | Retrieves the name of a file path.
fileName :: forall a s. Path a File s -> Name File
fileName (In _ f) = f
fileName _ = unsafeCrashWith
  """Hit unrechable path in Data.Pathy.fileName
  Based on type of this function, it must be called with a Path such that In node is a root node
  The reason might be a bug in this module or incorrect unsafeCoerce in it's use site
  """

-- | Retrieves the extension of a file name.
extension :: Name File -> String
extension (Name f) =
  let s = NEString.toString f
  in case S.lastIndexOf (S.Pattern ".") s of
    Just x -> S.drop (x + 1) s
    Nothing -> ""

-- | Drops the extension on a file name.
dropExtension :: Name File -> Maybe (Name File)
dropExtension (Name n) = 
  let
    s = NEString.toString n
  in case S.lastIndexOf (S.Pattern ".") s of
    Just x -> map Name $ NEString.fromString $ S.take x s
    Nothing -> Just (Name n)

changeExtension :: (String -> String) -> Name File -> Maybe (Name File)
changeExtension f nm =
  update (f $ extension nm) (dropExtension nm)
  where
  update ext' name = case NEString.fromString ext' of
    Nothing -> name
    Just ext -> Just $ _updateExt ext name

changeExtension' :: (String -> NonEmptyString) -> Name File -> Name File
changeExtension' f nm =
  _updateExt (f $ extension nm) (dropExtension nm)


_updateExt :: NonEmptyString -> Maybe (Name File) -> Name File
_updateExt ext = case _ of
  Just (Name n) -> Name $ n `appendString` "." <> ext
  Nothing -> Name ext

-- | Creates a path which points to a relative directory of the specified name.
dir :: forall s. NonEmptyString -> Path Rel Dir s
dir d = dir' (Name d)

-- | Creates a path which points to a relative directory of the specified name.
dir' :: forall s. Name Dir -> Path Rel Dir s
dir' d = In Current d

-- | Retrieves the name of a directory path. Not all paths have such a name,
-- | for example, the root or current directory.
dirName :: forall a s. Path a Dir s -> Maybe (Name Dir)
dirName p = case canonicalize p of
  In _ d -> Just d
  _ -> Nothing

pathName :: forall b s. AnyPath b s -> Either (Maybe (Name Dir)) (Name File)
pathName = bimap dirName fileName

-- | Given a directory path, appends either a file or directory to the path.
appendPath :: forall a b s. Path a Dir s -> Path Rel b s -> Path a b s
appendPath Current Current = Current
appendPath Root Current = Root
appendPath (ParentIn p1) Current = ParentIn (p1 </> Current)
appendPath (In p1 f1) Current = In (p1 </> Current) (unsafeCoerce $ f1)
appendPath p1 (ParentIn p2) = ParentIn (p1 </> p2)
appendPath p1 (In p2 f2) = In (p1 </> p2) f2
-- following cases don't make sense but cannot exist
appendPath Current Root = Current
appendPath Root Root = Root
appendPath (ParentIn p1) Root = ParentIn (p1 </> Current)
appendPath (In p1 f1) Root = In (p1 </> Current) (unsafeCoerce $ f1)

infixl 6 appendPath as </>

-- | Sets the extension of the file to the specified extension.
-- |
-- | ```purescript
-- | file "image" <.> "png"
-- | ```
setExtension :: forall a s. Path a File s -> NonEmptyString -> Path a File s
setExtension p ext = renameFile (changeExtension' $ const ext) p

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

-- | Peels off the last directory and the terminal file or directory name
-- | from the path. Returns `Nothing` if there is no such pair (for example,
-- | if the last path segment is root directory, current directory, or parent
-- | directory).
peel
  :: forall a b s
   . Path a b s
  -> Maybe (Tuple (Path a Dir s) (Name b))
peel Current = Nothing
peel Root = Nothing
peel p@(ParentIn _) = case canonicalize' p of
  Tuple true p' -> peel p'
  _ -> Nothing
peel (In p n) = Just $ Tuple p n

-- | Returns the depth of the path. This may be negative in some cases, e.g.
-- | `./../../../` has depth `-3`.
depth :: forall a b s. Path a b s -> Int
depth Current = 0
depth Root = 0
depth (ParentIn p) = depth p - 1
depth (In p _) = depth p + 1

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
unsandbox (In p n) = In (unsandbox p) n

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
renameFile :: forall a s. (Name File -> Name File) -> Path a File s -> Path a File s
renameFile f = un Identity <<< renameFile' (pure <<< f)

renameFile' :: forall f a s. Applicative f => (Name File -> f (Name File)) -> Path a File s -> f (Path a File s)
renameFile' f (In p f0) = In p <$> f f0 
renameFile' _ p = pure p

-- | Renames a directory path. Note: This is a simple rename of the terminal
-- | directory name, not a "move".
renameDir :: forall a s. (Name Dir -> Name Dir) -> Path a Dir s -> Path a Dir s
renameDir f (In p d) = In p (f d)
renameDir _ p = p

-- | Canonicalizes a path, by reducing things in the form `/x/../` to just `/x/`.
canonicalize :: forall a b s. Path a b s -> Path a b s
canonicalize = snd <<< canonicalize'

-- | Canonicalizes a path and returns information on whether or not it actually changed.
canonicalize' :: forall a b s. Path a b s -> Tuple Boolean (Path a b s)
canonicalize' Current = Tuple false Current
canonicalize' Root = Tuple false Root
canonicalize' (ParentIn (In p f)) = Tuple true  (unsafeCoerceType $ snd $ canonicalize' p)
canonicalize' (ParentIn p) = case canonicalize' p of
  Tuple changed p' ->
    let p'' = ParentIn p'
    in if changed then canonicalize' p'' else Tuple changed p''
canonicalize' (In p f) = flip In f <$> canonicalize' p

unsafePrintPath' :: forall a b s. SplitDirOrFile b => Escaper -> Path a b s -> String
unsafePrintPath' r = go
  where
    go :: forall a' b' s'. SplitDirOrFile b' => Path a' b' s' -> String
    go Current = "./"
    go Root = "/"
    go (ParentIn p) = go p <> "../"
    go (In p n) = case dirOrFileName n of
      Left dirN -> go p <> escape (runName dirN) <> "/"
      Right fileN -> go p <> escape (runName fileN)
    escape = runEscaper r

unsafePrintPath :: forall a b s. SplitDirOrFile b => Path a b s -> String
unsafePrintPath = unsafePrintPath' posixEscaper

-- | Prints a `Path` into its canonical `String` representation. For security
-- | reasons, the path must be sandboxed before it can be rendered to a string.
printPath :: forall a b. SplitDirOrFile b => Path a b Sandboxed -> String
printPath = unsafePrintPath

-- | Prints a `Path` into its canonical `String` representation, using the
-- | specified escaper to escape special characters in path segments. For
-- | security reasons, the path must be sandboxed before rendering to string.
printPath' :: forall a b. SplitDirOrFile b => Escaper -> Path a b Sandboxed -> String
printPath' = unsafePrintPath'

-- | Determines if two paths have the exact same representation. Note that
-- | two paths may represent the same path even if they have different
-- | representations!
identicalPath :: forall a a' b b' s s'. SplitDirOrFile b => SplitDirOrFile b' => Path a b s -> Path a' b' s' -> Boolean
identicalPath p1 p2 = show p1 == show p2

-- | Makes one path relative to another reference path, if possible, otherwise
-- | returns `Nothing`. The returned path inherits the sandbox settings of the
-- | reference path.
-- |
-- | Note there are some cases this function cannot handle.
relativeTo :: forall a b s s'. SplitDirOrFile b => Path a b s -> Path a Dir s' -> Maybe (Path Rel b s')
relativeTo p1 p2 = relativeTo' (canonicalize p1) (canonicalize p2)
  where
  relativeTo' :: forall b'. SplitDirOrFile b' => Path a b' s -> Path a Dir s' -> Maybe (Path Rel b' s')
  relativeTo' Root Root = pure Current
  relativeTo' Current Current = pure Current
  relativeTo' cp1 cp2
    | identicalPath cp1 cp2 = pure Current
    | otherwise = do
      Tuple cp1Path name <- peel cp1
      rel <- relativeTo' cp1Path cp2
      pure $ overName name
        (\dirN -> rel </> In Current dirN)
        (\fileN -> rel </> In Current fileN)
  overName 
    :: forall n a' s''
    .  SplitDirOrFile n 
    => Name n
    -> (Name Dir -> Path a' Dir s'')
    -> (Name File -> Path a' File s'')
    -> Path a' n s''
  overName p onDir onFile = case dirOrFileName p of
    Left p' -> unsafeCoerce $ onDir p'
    Right p' -> unsafeCoerce $ onFile p'

-- | Attempts to sandbox a path relative to some directory. If successful, the sandboxed
-- | directory will be returned relative to the sandbox directory (although this can easily
-- | be converted into an absolute path using `</>`).
-- |
-- | This combinator can be used to ensure that paths which originate from user-code
-- | cannot access data outside a given directory.
sandbox :: forall a b s. SplitDirOrFile b => Path a Dir Sandboxed -> Path a b s -> Maybe (Path Rel b Sandboxed)
sandbox p1 p2 = p2 `relativeTo` p1

-- | Refines path segments but does not change anything else.
refine :: forall a b s. SplitDirOrFile b => (Name File -> Name File) -> (Name Dir -> Name Dir) -> Path a b s -> Path a b s
refine f d = go
  where
    go :: forall a' b' s'. SplitDirOrFile b' => Path a' b' s' -> Path a' b' s'
    go Current = Current
    go Root = Root
    go (ParentIn p) = ParentIn (go p)
    go (In p name) = case dirOrFileName name of
       Left dirN ->
      -- We need to unwrap name so it compiles :((
       let Name n = (d dirN) in In (go p) (Name n)
       Right fileN ->
      -- We need to unwrap name so it compiles :((
       let Name n = (f fileN) in In (go p) (Name n)

type ParseError = Unit

-- | Parses a canonical `String` representation of a path into a `Path` value.
-- | Note that in order to be unambiguous, trailing directories should be
-- | marked with a trailing slash character (`'/'`).
parsePath
  :: forall z
   . (RelDir Unsandboxed -> z)
  -> (AbsDir Unsandboxed -> z)
  -> (RelFile Unsandboxed -> z)
  -> (AbsFile Unsandboxed -> z)
  -> (ParseError -> z)
  -> String
  -> z
parsePath rd ad rf af err "" = err unit
parsePath rd ad rf af err "/" = ad Root
parsePath rd ad rf af err p =
  let
    isAbs = S.take 1 p == "/"
    isFile = S.takeRight 1 p /= "/"
    segsRaw = S.split (S.Pattern "/") p
    segsDropped =
      -- drop last or/and first empty segment(s) if any
      case isAbs, isFile of
        true, true -> drop 1 $ segsRaw
        true, false -> drop 1 $ dropEnd 1 segsRaw
        false, true -> segsRaw
        false, false -> dropEnd 1 segsRaw
    last = length segsDropped - 1
    folder :: forall a b s. Int -> Path a b s -> NonEmptyString -> Path a b s
    folder idx base seg =
      if NEString.toString seg == "." then
        base
      else if NEString.toString seg == ".." then
        ParentIn base
      else In (unsafeCoerceType base) (Name seg)
  in
    case traverse NEString.fromString segsDropped of
      Nothing -> err unit
      Just segs -> case isAbs, isFile of
        true, true -> af $ foldlWithIndex folder Root segs
        true, false -> ad $ foldlWithIndex folder Root segs
        false, true -> rf $ foldlWithIndex folder Current segs
        false, false -> rd $ foldlWithIndex folder Current segs

-- | Attempts to parse a relative file from a string.
parseRelFile :: String -> Maybe (RelFile Unsandboxed)
parseRelFile = parsePath (const Nothing) (const Nothing) Just (const Nothing) (const Nothing)

-- | Attempts to parse an absolute file from a string.
parseAbsFile :: String -> Maybe (AbsFile Unsandboxed)
parseAbsFile = parsePath (const Nothing) (const Nothing) (const Nothing) Just (const Nothing)

-- | Attempts to parse a relative directory from a string.
parseRelDir :: String -> Maybe (RelDir Unsandboxed)
parseRelDir = parsePath Just (const Nothing) (const Nothing) (const Nothing) (const Nothing)

-- | Attempts to parse an absolute directory from a string.
parseAbsDir :: String -> Maybe (AbsDir Unsandboxed)
parseAbsDir = parsePath (const Nothing) Just (const Nothing) (const Nothing) (const Nothing)

instance showPath :: SplitDirOrFile b => Show (Path a b s) where
  show Current = "currentDir"
  show Root = "rootDir"
  show (ParentIn p) = "(parentDir' " <> show p <> ")"
  show (In p n ) = case dirOrFileName n of
    Left dirN -> 
    "(" <> show p <> " </> dir " <> show dirN <> ")"
    Right fileN -> 
    "(" <> show p <> " </> file " <> show fileN <> ")"

instance eqPath :: SplitDirOrFile b => Eq (Path a b s) where
  eq p1 p2 = canonicalize p1 `identicalPath` canonicalize p2

instance ordPath :: SplitDirOrFile b => Ord (Path a b s) where
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
    go (In p1' d1) (In p2' d2) = compare p1' p2' <> compare d1 d2

instance showName :: Show (Name a) where
  show (Name name) = "(Name " <> show name <> ")"

derive instance eqName :: Eq (Name a)
derive instance ordName :: Ord (Name a)
