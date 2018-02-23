module Pathy.Path
  ( Path
  , AnyPath
  , RelPath
  , AbsPath
  , RelDir
  , AbsDir
  , RelFile
  , AbsFile
  , rootDir
  , currentDir
  , dir
  , dir'
  , file
  , file'
  , parentOf
  , extendPath
  , appendPath, (</>)
  , parentAppend, (<..>)
  , canonicalize
  , foldPath
  , peel
  , peelFile
  , name
  , fileName
  , rename
  , renameTraverse
  , setExtension, (<.>)
  , relativeTo
  , refine
  ) where

import Prelude

import Data.Either (Either)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.String.NonEmpty as NES
import Data.Symbol (SProxy)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Pathy.Name (class IsName, Name(..), alterExtension, reflectName)
import Pathy.Phantom (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, File, Rel, foldDirOrFile, foldRelOrAbs, onDirOrFile, kind DirOrFile, kind RelOrAbs)
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
-- | possible way for such paths to be constructed by user-land code. The only
-- | "invalid path" that may be constructed is using the `parentOf` function,
-- | e.g. `parentOf rootDir`, or by parsing an equivalent string such as
-- | `/../`, but such paths may not be rendered to strings until they are first
-- | sandboxed to some directory.
data Path (a :: RelOrAbs) (b :: DirOrFile)
  = Init
  | ParentOf (Path a Dir)
  | In (Path a Dir) (Name b)

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

-- | A type describing a file whose location is given relative to some other,
-- | unspecified directory (referred to as the "current directory").
type RelFile = Path Rel File

-- | A type describing a file whose location is absolutely specified.
type AbsFile = Path Abs File

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
file :: forall s. IsName s => SProxy s -> Path Rel File
file = file' <<< reflectName

-- | Creates a path which points to a relative file of the specified name.
file' :: Name File -> Path Rel File
file' = In currentDir

-- | Creates a path which points to a relative directory of the specified name.
-- |
-- | Instead of accepting a runtime value, this function accepts a type-level
-- | string via a proxy, to ensure the constructed name is not empty.
dir :: forall s. IsName s => SProxy s -> Path Rel Dir
dir = dir' <<< reflectName

-- | Creates a path which points to a relative directory of the specified name.
dir' :: Name Dir -> Path Rel Dir
dir' = In currentDir

-- | Creates a path that points to the parent directory of the specified path.
parentOf :: forall a. Path a Dir -> Path a Dir
parentOf p = ParentOf p

-- | Extends a path with a file or directory under the current path.
extendPath :: forall a b. Path a Dir -> Name b -> Path a b
extendPath p = In p

-- | Given a directory path, appends a relative path to extend the original
-- | path.
appendPath :: forall a b. Path a Dir -> Path Rel b -> Path a b
appendPath = case _, _ of
  Init, Init -> Init
  ParentOf p, Init -> ParentOf (p </> Init)
  In p (Name d), Init -> In (p </> Init) (Name d)
  p1, ParentOf p2 -> ParentOf (p1 </> p2)
  p1, In p2 n -> In (p1 </> p2) n

infixl 6 appendPath as </>

-- | Ascends into the parent of the specified directory, then descends into
-- | the specified path.
-- |
-- | ```purescript
-- | canonicalize (rootDir </> dir "foo" <..> dir "bar") = rootDir </> dir "bar"
-- | ```
parentAppend :: forall a b. Path a Dir -> Path Rel b -> Path a b
parentAppend d p = parentOf d </> p

infixl 6 parentAppend as <..>

-- | Canonicalizes a path, by reducing things in the form `/x/../` to just
-- | `/x/`. Paths like `/../` will be normalized to `/`.
canonicalize :: forall a b. IsRelOrAbs a => Path a b -> Path a b
canonicalize p = fromMaybe p (go p)
  where
    go :: forall b'. Path a b' -> Maybe (Path a b')
    go = case _ of
      Init ->
        Nothing
      p'@(ParentOf Init) ->
        foldRelOrAbs
          (const Nothing)
          (const (Just Init)) -- This normalizes `/../` case into `/`
          p'
      ParentOf (In p' _) ->
        -- Coercion is safe as `ParentOf` can only appear where `b' ~ Dir`
        Just $ (unsafeCoerce :: Path a Dir -> Path a b') (canonicalize p')
      ParentOf p'@(ParentOf _) ->
        case go p' of
          Just p'' -> Just $ canonicalize (ParentOf p'')
          Nothing -> Nothing
      In p' n ->
        flip In n <$> go p'

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
  -> (Path a Dir -> r)
  -> (Path a Dir -> Name b -> r)
  -> Path a b
  -> r
foldPath r f g = case _ of
  Init -> r
  ParentOf d -> f d
  In d n -> g d n

-- | Peels off the last directory and the terminal file or directory name
-- | from the path. Returns `Nothing` if the path is `rootDir` / `currentDir` or
-- | some `parentOf p`, so you might wanna [canonicalize](#v:canonicalize) first,
-- | or use [`peel'`](#v:peel').
peel :: forall a b. Path a b -> Maybe (Tuple (Path a Dir) (Name b))
peel = foldPath Nothing (const Nothing) (\p n -> Just (Tuple p n))

-- | Same as [`peel`](#v:peel) but input is first [canonicalized](#v:canonicalize).
peel' :: forall a b. IsRelOrAbs a => Path a b -> Maybe (Tuple (Path a Dir) (Name b))
peel' = canonicalize >>> peel

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
fileName = case _ of
  Init -> unsafeCrashWith "`Init` in Pathy.fileName (this should be impossible)"
  ParentOf _ -> unsafeCrashWith "`ParentOf` in Pathy.fileName (this should be impossible)"
  In _ n -> n

-- | Attempts to rename the terminal segment of a path. If the path is
-- | `rootDir` / `currentDir` or some `parentOf p` this will have no effect.
rename :: forall a b. (Name b -> Name b) -> Path a b -> Path a b
rename f = un Identity <<< renameTraverse (pure <<< f)

-- | Attempts to rename the terminal segment of a path using a function that
-- | returns the result in some `Applicative`. If the path is `rootDir` /
-- | `currentDir` or some `parentOf p` this will have no effect.
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
-- | `rootDir` / `currentDir` or some `parentOf p` this will have no effect. If
-- | the passed string is empty, this will remove any existing extension.
-- |
-- | ```purescript
-- | file "image" <.> "png"
-- | ```
setExtension :: forall a b. Path a b -> String -> Path a b
setExtension p ext = rename (alterExtension (const (NES.fromString ext))) p

infixl 6 setExtension as <.>

-- | Makes a path relative to a reference path.
relativeTo :: forall b. Path Abs b -> Path Abs Dir -> Path Rel b
relativeTo p rp = coeB $ step Init (canonicalize (coeD p)) (canonicalize rp)
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
