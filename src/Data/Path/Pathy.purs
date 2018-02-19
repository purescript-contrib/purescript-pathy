module Data.Path.Pathy
  ( Abs
  , AbsDir
  , ParseError
  , AbsFile
  , AbsPath
  , Dir
  , Name(..)
  , File
  , Path
  , AnyPath
  , Rel
  , RelDir
  , RelFile
  , RelPath
  , appendPath
  , (</>)
  , setExtension
  , (<.>)
  , parentAppend
  , (<..>)
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
  , foldPath
  , peel
  , parsePath
  , parseAbsDir
  , parseAbsFile
  , parseRelDir
  , parseRelFile
  , class IsRelOrAbs
  , onRelOrAbs
  , foldRelOrAbs
  , class IsDirOrFile
  , onDirOrFile
  , foldDirOrFile
  , refine
  , relativeTo
  , renameDir
  , renameFile
  , renameFile'
  , rootDir
  , DirPathView
  , FilePathView
  , viewDir
  , viewFile
  , peelFile
  )
  where

import Prelude

import Data.Array (drop, dropEnd, length)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Identity (Identity(..))
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.String as S
import Data.String.NonEmpty (NonEmptyString, appendString)
import Data.String.NonEmpty (fromString, toString) as NEString
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

foreign import kind RelOrAbs

foreign import kind DirOrFile

-- | The (phantom) type of relative paths.
foreign import data Rel :: RelOrAbs

-- | The (phantom) type of absolute paths.
foreign import data Abs :: RelOrAbs

-- | The (phantom) type of files.
foreign import data File :: DirOrFile

-- | The (phantom) type of directories.
foreign import data Dir :: DirOrFile

-- | A newtype around a file name.
newtype Name (n :: DirOrFile) = Name NonEmptyString

derive instance newtypeName :: Newtype (Name n) _

-- | A type that describes a Path. All flavors of paths are described by this
-- | type, whether they are absolute or relative paths and whether they
-- | refer to files or directories.
-- |
-- | * The type parameter `a` describes whether the path is `Rel` or `Abs`.
-- | * The type parameter `b` describes whether the path is `File` or `Dir`.
-- |
-- | To ensure type safety, there is no way for users to create a value of
-- | this type directly. Instead, helpers should be used, such as `rootDir`,
-- | `currentDir`, `file`, `dir`,  `(</>)`, and `parsePath`.
-- |
-- | This ADT allows invalid paths (e.g. paths inside files), but there is no
-- | possible way for such paths to be constructed by user-land code. The only
-- | "invalid path" that may be constructed is using the `parentDir` function,
-- | e.g. `parentDir rootDir`, or by parsing an equivalent string such as
-- | `/../`, but such paths may not be rendered to strings until they are first
-- | sandboxed to some directory.
data Path (a :: RelOrAbs) (b :: DirOrFile)
  = Init
  | ParentIn (Path a Dir)
  | In (Path a Dir) (Name b)

-- | A type describing a file whose location is given relative to some other,
-- | unspecified directory (referred to as the "current directory").
type RelFile = Path Rel File

-- | A type describing a file whose location is absolutely specified.
type AbsFile = Path Abs File

-- | A type describing a directory whose location is given relative to some
-- | other, unspecified directory (referred to as the "current directory").
type RelDir = Path Rel Dir

-- | A type describing a directory whose location is absolutely specified.
type AbsDir = Path Abs Dir

-- | A type describing a file or directory path.
type AnyPath b = Either (Path b Dir) (Path b File)

-- | A type describing a relative file or directory path.
type RelPath = AnyPath Rel

-- | A type describing an absolute file or directory path.
type AbsPath = AnyPath Abs

class IsDirOrFile (b :: DirOrFile) where
  onDirOrFile
    :: forall f r
     . ((f Dir -> f b) -> f Dir -> r)
    -> ((f File -> f b) -> f File -> r)
    -> f b
    -> r

foldDirOrFile :: forall f b r. IsDirOrFile b => (f Dir -> r) -> (f File -> r) -> f b -> r
foldDirOrFile f g = onDirOrFile (const f) (const g)

instance relIsDirOrFile :: IsDirOrFile Dir where onDirOrFile f _ = f id
instance absIsDirOrFile :: IsDirOrFile File where onDirOrFile _ f = f id

class IsRelOrAbs (a :: RelOrAbs) where
  onRelOrAbs
    :: forall f b r
    . ((f Rel b -> f a b) -> f Rel b -> r)
    -> ((f Abs b -> f a b) -> f Abs b -> r)
    -> f a b
    -> r

instance relIsRelOrAbs :: IsRelOrAbs Rel where onRelOrAbs f _ = f id
instance absIsRelOrAbs :: IsRelOrAbs Abs where onRelOrAbs _ f = f id

foldRelOrAbs
  :: forall f a b r
  . IsRelOrAbs a
  => (f Rel b -> r)
  -> (f Abs b -> r)
  -> f a b
  -> r
foldRelOrAbs f g = onRelOrAbs (const f) (const g)

-- | Creates a path which points to a relative file of the specified name.
file :: NonEmptyString -> Path Rel File
file = file' <<< Name

-- | Creates a path which points to a relative file of the specified name.
file' :: Name File -> Path Rel File
file' = In Init

-- | Retrieves the name of a file path.
fileName :: forall a. Path a File -> Name File
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
dir :: NonEmptyString -> Path Rel Dir
dir = dir' <<< Name

-- | Creates a path which points to a relative directory of the specified name.
dir' :: Name Dir -> Path Rel Dir
dir' = In Init

-- | Retrieves the name of a directory path. Not all paths have such a name,
-- | for example, the root or current directory.
dirName :: forall a. Path a Dir -> Maybe (Name Dir)
dirName p = case canonicalize p of
  In _ d -> Just d
  _ -> Nothing

pathName :: forall b. AnyPath b -> Either (Maybe (Name Dir)) (Name File)
pathName = bimap dirName fileName

-- | Given a directory path, appends either a file or directory to the path.
appendPath :: forall a b. Path a Dir -> Path Rel b -> Path a b
appendPath Init Init = Init
appendPath (ParentIn p) Init = ParentIn (p </> Init)
appendPath (In p (Name d)) Init = In (p </> Init) (Name d)
appendPath p1 (ParentIn p2) = ParentIn (p1 </> p2)
appendPath p1 (In p2 n2) = In (p1 </> p2) n2

infixl 6 appendPath as </>

-- | Sets the extension of the file to the specified extension.
-- |
-- | ```purescript
-- | file "image" <.> "png"
-- | ```
setExtension :: forall a s. Path a File -> NonEmptyString -> Path a File
setExtension p ext = renameFile (changeExtension' $ const ext) p

infixl 6 setExtension as <.>

-- | Ascends into the parent of the specified directory, then descends into
-- | the specified path.
parentAppend :: forall a b. Path a Dir -> Path Rel b -> Path a b
parentAppend d p = parentDir d </> p

infixl 6 parentAppend as <..>

foldPath
  :: forall a b r
   . r
  -> (Path a Dir -> r)
  -> (Path a Dir -> Name b -> r)
  -> Path a b
  -> r
foldPath r f g = case _ of
  Init -> r
  ParentIn d -> f d
  In d n -> g d n

-- | Peels off the last directory and the terminal file or directory name
-- | from the path. Returns `Nothing` if there is no such pair (for example,
-- | if the last path segment is root directory, current directory, or parent
-- | directory).
peel
  :: forall a b
   . Path a b
  -> Maybe (Tuple (Path a Dir) (Name b))
peel Init = Nothing
peel p@(ParentIn _) = case canonicalize' p of
  Tuple true p' -> peel p'
  _ -> Nothing
peel (In p n) = Just $ Tuple p n

-- | Returns the depth of the path. This may be negative in some cases, e.g.
-- | `./../../../` has depth `-3`.
depth :: forall a b. Path a b -> Int
depth Init = 0
depth (ParentIn p) = depth p - 1
depth (In p _) = depth p + 1

-- | Creates a path that points to the parent directory of the specified path.
parentDir :: forall a. Path a Dir -> Path a Dir
parentDir = ParentIn

unsafeCoerceType :: forall a b b'. Path a b -> Path a b'
unsafeCoerceType = unsafeCoerce

  -- | The "current directory", which can be used to define relatively-located resources.
currentDir :: Path Rel Dir
currentDir = Init

-- | The root directory, which can be used to define absolutely-located resources.
rootDir :: Path Abs Dir
rootDir = Init

-- | Renames a file path.
renameFile :: forall a. (Name File -> Name File) -> Path a File -> Path a File
renameFile f = un Identity <<< renameFile' (pure <<< f)

renameFile' :: forall f a s. Applicative f => (Name File -> f (Name File)) -> Path a File -> f (Path a File)
renameFile' f (In p f0) = In p <$> f f0
renameFile' _ p = pure p

-- | Renames a directory path. Note: This is a simple rename of the terminal
-- | directory name, not a "move".
renameDir :: forall a. (Name Dir -> Name Dir) -> Path a Dir -> Path a Dir
renameDir f (In p d) = In p (f d)
renameDir _ p = p

-- | Canonicalizes a path, by reducing things in the form `/x/../` to just `/x/`.
canonicalize :: forall a b. Path a b -> Path a b
canonicalize = snd <<< canonicalize'

-- | Canonicalizes a path and returns information on whether or not it actually changed.
canonicalize' :: forall a b. Path a b -> Tuple Boolean (Path a b)
canonicalize' Init = Tuple false Init
canonicalize' (ParentIn (In p f)) = Tuple true (unsafeCoerceType $ snd $ canonicalize' p)
canonicalize' (ParentIn p) = case canonicalize' p of
  Tuple changed p' ->
    let p'' = ParentIn p'
    in if changed then canonicalize' p'' else Tuple changed p''
canonicalize' (In p f) = flip In f <$> canonicalize' p

-- | Determines if two paths have the exact same representation. Note that
-- | two paths may represent the same path even if they have different
-- | representations!
identicalPath
  :: forall a a' b b'
   . IsRelOrAbs a
  => IsRelOrAbs a'
  => IsDirOrFile b
  => IsDirOrFile b'
  => Path a b -> Path a' b' -> Boolean
identicalPath p1 p2 = show p1 == show p2

-- | Makes one path relative to another reference path, if possible, otherwise
-- | returns `Nothing`.
-- |
-- | Note there are some cases this function cannot handle.
relativeTo :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> Path a Dir -> Maybe (Path Rel b)
relativeTo p1 p2 = relativeTo' (canonicalize p1) (canonicalize p2)
  where
  relativeTo' :: forall b'. IsDirOrFile b' => Path a b' -> Path a Dir -> Maybe (Path Rel b')
  relativeTo' Init Init = pure Init
  relativeTo' cp1 cp2
    | identicalPath cp1 cp2 = pure Init
    | otherwise = do
        Tuple cp1Path name <- peel cp1
        rel <- relativeTo' cp1Path cp2
        pure $ rel </> In Init name

-- | Refines path segments but does not change anything else.
refine :: forall a b. IsDirOrFile b => (Name File -> Name File) -> (Name Dir -> Name Dir) -> Path a b -> Path a b
refine f d = go
  where
    go :: forall a' b'. IsDirOrFile b' => Path a' b' -> Path a' b'
    go Init = Init
    go (ParentIn p) = ParentIn (go p)
    go (In p name) = In (go p) (onDirOrFile (\p -> p <<< d) (\p -> p <<< f) name)

type ParseError = Unit

-- | Parses a canonical `String` representation of a path into a `Path` value.
-- | Note that in order to be unambiguous, trailing directories should be
-- | marked with a trailing slash character (`'/'`).
parsePath
  :: forall z
   . (RelDir -> z)
  -> (AbsDir -> z)
  -> (RelFile -> z)
  -> (AbsFile -> z)
  -> (ParseError -> z)
  -> String
  -> z
parsePath rd ad rf af err "" = err unit
parsePath rd ad rf af err "/" = ad Init
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
    folder :: forall a b. IsDirOrFile b => Int -> Path a b -> NonEmptyString -> Path a b
    folder idx base seg =
      if NEString.toString seg == "." then
        base
      else if NEString.toString seg == ".." then
        ParentIn $ unsafeCoerceType base
      else In (unsafeCoerceType base) (Name seg)
  in
    case traverse NEString.fromString segsDropped of
      Nothing -> err unit
      Just segs -> case isAbs, isFile of
        true, true -> af $ foldlWithIndex folder Init segs
        true, false -> ad $ foldlWithIndex folder Init segs
        false, true -> rf $ foldlWithIndex folder Init segs
        false, false -> rd $ foldlWithIndex folder Init segs

-- | Attempts to parse a relative file from a string.
parseRelFile :: String -> Maybe (RelFile)
parseRelFile = parsePath (const Nothing) (const Nothing) Just (const Nothing) (const Nothing)

-- | Attempts to parse an absolute file from a string.
parseAbsFile :: String -> Maybe (AbsFile)
parseAbsFile = parsePath (const Nothing) (const Nothing) (const Nothing) Just (const Nothing)

-- | Attempts to parse a relative directory from a string.
parseRelDir :: String -> Maybe (RelDir)
parseRelDir = parsePath Just (const Nothing) (const Nothing) (const Nothing) (const Nothing)

-- | Attempts to parse an absolute directory from a string.
parseAbsDir :: String -> Maybe (AbsDir)
parseAbsDir = parsePath (const Nothing) Just (const Nothing) (const Nothing) (const Nothing)

instance showPathRelDir :: (IsRelOrAbs a, IsDirOrFile b) => Show (Path a b) where
  show p@Init = foldRelOrAbs (const "currentDir") (const "rootDir") p
  show (ParentIn p) = "(parentDir " <> show p <> ")"
  show (In p n) = "(" <> show p <> " </> " <> foldDirOrFile (("dir " <> _) <<< show) (("file " <> _) <<< show) n <> ")"

instance eqPath :: (IsRelOrAbs a, IsDirOrFile b) => Eq (Path a b) where
  eq p1 p2 = canonicalize p1 `identicalPath` canonicalize p2

instance ordPath :: (IsRelOrAbs a, IsDirOrFile b) => Ord (Path a b) where
  compare p1 p2 = go (canonicalize p1) (canonicalize p2)
    where
    go Init Init = EQ
    go Init _ = LT
    go _ Init = GT
    go (ParentIn p1') (ParentIn p2') = compare p1' p2'
    go (ParentIn _) _ = LT
    go _ (ParentIn _) = GT
    go (In p1' d1) (In p2' d2) = compare p1' p2' <> compare d1 d2

instance showName :: Show (Name a) where
  show (Name name) = "(Name " <> show name <> ")"

derive instance eqName :: Eq (Name a)
derive instance ordName :: Ord (Name a)

type DirPathView = List (Name Dir)
type FilePathView = Tuple DirPathView (Name File)

viewDir :: forall a. Path a Dir -> DirPathView
viewDir = reverse <<< go
  where
  go = case _ of
    Init -> Nil
    ParentIn _ -> unsafeCrashWith "Impossible, ParentIn can't be in path"
    In d n -> Cons n (go d)

viewFile :: forall a. Path a File -> FilePathView
viewFile = peelFile >>> lmap viewDir

peelFile :: forall a. Path a File -> Tuple (Path a Dir) (Name File)
peelFile = case _ of
  Init -> unsafeCrashWith "Impossible, Init can't be in File path"
  ParentIn _ -> unsafeCrashWith "Impossible, ParentIn can't be in File path"
  In d n -> Tuple d n
