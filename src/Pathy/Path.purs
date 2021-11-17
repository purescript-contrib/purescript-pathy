module Pathy.Path
  ( Path
  , AnyPath
  , RelPath
  , AbsPath
  , RelDir
  , AbsDir
  , AnyDir
  , RelFile
  , AbsFile
  , AnyFile
  , rootDir
  , currentDir
  , dir
  , dir'
  , file
  , file'
  , in'
  , parentOf
  , extendPath
  , appendPath
  , (</>)
  , parentAppend
  , (<..>)
  , foldPath
  , peel
  , peelFile
  , name
  , fileName
  , rename
  , renameTraverse
  , setExtension
  , (<.>)
  , relativeTo
  , refine
  ) where

import Prelude

import Data.Either (Either)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.String.NonEmpty as NES
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import Pathy.Name (class IsName, Name(..), alterExtension, reflectName)
import Pathy.Phantom (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, File, Rel, foldDirOrFile, foldRelOrAbs, onDirOrFile, onRelOrAbs, DirOrFile, RelOrAbs)
import Unsafe.Coerce (unsafeCoerce)

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
-- | possible way for such paths to be constructed by user-land code.
data Path :: RelOrAbs -> DirOrFile -> Type
data Path a b
  = Init
  | ParentOf (Path Rel Dir)
  | In (Path a Dir) (Name b)

type role Path nominal nominal

derive instance eqPath :: Eq (Path a b)
derive instance ordPath :: Ord (Path a b)

instance showPathRelDir :: (IsRelOrAbs a, IsDirOrFile b) => Show (Path a b) where
  show p@Init = foldRelOrAbs (const "currentDir") (const "rootDir") p
  show (ParentOf p) = "(parentOf " <> show p <> ")"
  show (In p n) = "(" <> show p <> " </> " <> foldDirOrFile (("dir " <> _) <<< show) (("file " <> _) <<< show) n <> ")"

-- | A type describing a file or directory path.
type AnyPath a = Either (Path a Dir) (Path a File)

-- | A type describing a relative file or directory path.
type RelPath = AnyPath Rel

-- | A type describing an absolute file or directory path.
type AbsPath = AnyPath Abs

-- | A type describing a directory whose location is given relative to some
-- | other, unspecified directory (referred to as the "current directory").
type RelDir = Path Rel Dir

-- | A type describing a directory whose location is absolutely specified.
type AbsDir = Path Abs Dir

-- | A type describing a absolute or relative directory path.
type AnyDir = Either AbsDir RelDir

-- | A type describing a file whose location is given relative to some other,
-- | unspecified directory (referred to as the "current directory").
type RelFile = Path Rel File

-- | A type describing a file whose location is absolutely specified.
type AbsFile = Path Abs File

-- | A type describing a absolute or relative file path.
type AnyFile = Either AbsFile RelFile

-- | The root directory, which can be used to define absolutely-located resources.
rootDir :: Path Abs Dir
rootDir = Init

-- | The "current directory", which can be used to define relatively-located
-- | resources.
currentDir :: Path Rel Dir
currentDir = Init

-- | Creates a path which points to a relative file of the specified name.
-- |
-- | Instead of accepting a runtime value, this function accepts a type-level
-- | string via a proxy, to ensure the constructed name is not empty.
file :: forall s proxy. IsName s => proxy s -> Path Rel File
file = file' <<< reflectName

-- | Creates a path which points to a relative file of the specified name.
file' :: Name File -> Path Rel File
file' = in'

-- | Creates a path which points to a relative directory of the specified name.
-- |
-- | Instead of accepting a runtime value, this function accepts a type-level
-- | string via a proxy, to ensure the constructed name is not empty.
dir :: forall s proxy. IsName s => proxy s -> Path Rel Dir
dir = dir' <<< reflectName

-- | Creates a path which points to a relative directory of the specified name.
dir' :: Name Dir -> Path Rel Dir
dir' = in'

-- | Creates a path which points to a relative directory or file of the specified name.
-- | In most cases [`dir'`](#v:dir') or [`file'`](#v:file') should be used instead,
-- | but it's still there in case the segment type is going to be determined based
-- | on some type variable.
-- |
-- | ``` purescript
-- | p == maybe p (\(Tuple r n) -> r </> in' n) (peel p)
-- | ```
in' :: forall a. Name a -> Path Rel a
in' = In currentDir

-- | Creates a path that points to the parent directory of the specified path.
-- |
-- | Calling `parentOf` on `rootDir` will return `rootDir`.
parentOf :: forall a. IsRelOrAbs a => Path a Dir -> Path a Dir
parentOf =
  onRelOrAbs
    (\coe p -> maybe (ParentOf p) (coe <<< fst) (peel p))
    (\coe -> coe <<< maybe Init fst <<< peel)

-- | Extends a path with a file or directory under the current path.
extendPath :: forall a b. Path a Dir -> Name b -> Path a b
extendPath p = In p

-- | Given a directory path, appends a relative path to extend the original
-- | path.
appendPath :: forall a b. IsRelOrAbs a => Path a Dir -> Path Rel b -> Path a b
appendPath = case _, _ of
  Init, Init -> Init
  ParentOf p, Init -> ParentOf (p </> Init)
  In p (Name d), Init -> In (p </> Init) (Name d)
  p1, ParentOf p2 -> (unsafeCoerce :: Path a Dir -> Path a b) $ parentOf (p1 </> p2)
  p1, In p2 n -> In (p1 </> p2) n

infixl 6 appendPath as </>

-- | Ascends into the parent of the specified directory, then descends into
-- | the specified path.
-- |
-- | ```purescript
-- | rootDir </> dir "foo" <..> dir "bar" = rootDir </> dir "bar"
-- | ```
parentAppend :: forall a b. IsRelOrAbs a => Path a Dir -> Path Rel b -> Path a b
parentAppend d p = parentOf d </> p

infixl 6 parentAppend as <..>

-- | A fold over `Path`s. Since `Path` has private constructors, this allows for
-- | functions to be written over its constructors, similar to a total pattern
-- | match.
-- |
-- | - The first argument is the value to return for the `currentDir`/`rootDir`
-- |   at the base of the path.
-- | - The second argument is a function for handling a step into the parent
-- |   directory of the path it receives (eliminates `parentOf`).
-- | - The third argument is a function representing a file or directory within
-- |   the directory of the path it receives (eliminates `extendPath`).
foldPath
  :: forall a b r
   . r
  -> (Path Rel Dir -> r)
  -> (Path a Dir -> Name b -> r)
  -> Path a b
  -> r
foldPath r f g = case _ of
  Init -> r
  ParentOf d -> f d
  In d n -> g d n

-- | Peels off the last directory and the terminal file or directory name
-- | from the path. Returns `Nothing` if the path is `rootDir` / `currentDir` or
-- | a relative path that is ascending (`../`)
peel :: forall a b. Path a b -> Maybe (Tuple (Path a Dir) (Name b))
peel = foldPath Nothing (const Nothing) (\p n -> Just (Tuple p n))

-- | Peels off the last director and terminal file from a path. Unlike the
-- | general `peel` function this is guaranteed to return a result, as `File`
-- | paths are known to have a name.
peelFile :: forall a. Path a File -> Tuple (Path a Dir) (Name File)
peelFile = case _ of
  Init -> unsafeCrashWith "`Init` in Pathy.peelFile (this should be impossible)"
  ParentOf _ -> unsafeCrashWith "`ParentOf` in Pathy.peelFile (this should be impossible)"
  In p n -> Tuple p n

-- | Retrieves the name of the terminal segment in a path. Returns `Nothing` if
-- | the path is `rootDir` / `currentDir` or some `parentOf p`.
name :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> Maybe (Name b)
name = foldPath Nothing (const Nothing) (const Just)

-- | Retrieves the name of a file path. Unlike the general `name` function,
-- | this is guaranteed to return a result, as `File` paths are known to have a
-- | name.
fileName :: forall a. Path a File -> Name File
fileName = snd <<< peelFile

-- | Attempts to rename the terminal segment of a path. If the path is
-- | `rootDir` / `currentDir` or a relative path that is ascending (`../`) this
-- | will have no effect.
rename :: forall a b. (Name b -> Name b) -> Path a b -> Path a b
rename f = un Identity <<< renameTraverse (pure <<< f)

-- | Attempts to rename the terminal segment of a path using a function that
-- | returns the result in some `Applicative`. If the path is `rootDir` /
-- | `currentDir` or a relative path that is ascending (`../`) this will
-- | have no effect.
renameTraverse
  :: forall f a b
   . Applicative f
  => (Name b -> f (Name b))
  -> Path a b
  -> f (Path a b)
renameTraverse f = case _ of
  In p n -> In p <$> f n
  p -> pure p

-- | Sets the extension on the terminal segment of a path. If the path is
-- | `rootDir` / `currentDir` or a relative path that is ascending (`../`) this
-- | will have no effect.
-- |
-- | ```purescript
-- | file "image" <.> "png"
-- | ```
-- | See [`splitName`](Pathy.Name#v:splitName) and [`alterExtension`](Pathy.Name#v:alterExtension)
-- | fore more examples.
setExtension :: forall a b. Path a b -> String -> Path a b
setExtension p ext = rename (alterExtension (const (NES.fromString ext))) p

infixl 6 setExtension as <.>

-- | Makes a path relative to a reference path. This function is best
-- | explaned using this property:
-- |
-- | ```purescript
-- | a == r </> a `relativeTo` r
-- | ```
relativeTo :: forall b. Path Abs b -> Path Abs Dir -> Path Rel b
relativeTo p = coeB <<< step Init (coeD p)
  where
  step :: Path Rel Dir -> Path Abs Dir -> Path Abs Dir -> Path Rel Dir
  step acc = case _, _ of
    p', rp' | p' == rp' -> acc
    Init, In rp' _ -> step (ParentOf acc) Init rp'
    In p' n, Init -> In (step acc p' Init) n
    In p' n, rp'
      | p' == rp' -> In acc n
      | otherwise -> In (step acc p' rp') n
    _, _ ->
      unsafeCrashWith "`ParentOf` in Pathy.relativeTo (this should be impossible)"

  -- Unfortunately we can't avoid some coercions in this function unless
  -- we actually write two different verions of `relativeTo` for file/dir
  -- paths. Since the actual data representation is same either way the
  -- coercions are safe.
  coeD :: forall a. Path a b -> Path a Dir
  coeD = unsafeCoerce

  coeB :: forall a. Path a Dir -> Path a b
  coeB = unsafeCoerce

-- | Refines path segments but does not change anything else.
refine
  :: forall a b
   . IsDirOrFile b
  => (Name File -> Name File)
  -> (Name Dir -> Name Dir)
  -> Path a b
  -> Path a b
refine f d = go
  where
  go :: forall a' b'. IsDirOrFile b' => Path a' b' -> Path a' b'
  go Init = Init
  go (ParentOf p) = ParentOf (go p)
  go (In p n) = In (go p) (onDirOrFile (_ <<< d) (_ <<< f) n)
