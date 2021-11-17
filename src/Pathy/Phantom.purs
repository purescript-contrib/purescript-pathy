module Pathy.Phantom where

import Prelude

-- | The kind for the relative/absolute phantom type.
data RelOrAbs

-- | The phantom type of relative paths.
foreign import data Rel :: RelOrAbs

-- | The phantom type of absolute paths.
foreign import data Abs :: RelOrAbs

-- | A class that enables writing operations that abstract over `RelOrAbs`.
-- |
-- | The provided `onRelOrAbs` function folds over a value indexed by
-- | `RelOrAbs` to produce a new result, passing proof/coercion functions to
-- | allow the inner functions to unify their return types if remapping.
class IsRelOrAbs :: RelOrAbs -> Constraint
class IsRelOrAbs a where
  onRelOrAbs
    :: forall (f :: RelOrAbs -> DirOrFile -> Type) b r
     . ((f Rel b -> f a b) -> f Rel b -> r)
    -> ((f Abs b -> f a b) -> f Abs b -> r)
    -> f a b
    -> r

instance relIsRelOrAbs :: IsRelOrAbs Rel where
  onRelOrAbs f _ = f identity

instance absIsRelOrAbs :: IsRelOrAbs Abs where
  onRelOrAbs _ f = f identity

-- | Folds over a value that uses `RelOrAbs` to produce a new result.
foldRelOrAbs
  :: forall f a b r
   . IsRelOrAbs a
  => (f Rel b -> r)
  -> (f Abs b -> r)
  -> f a b
  -> r
foldRelOrAbs f g = onRelOrAbs (const f) (const g)

-- | The kind for the directory/file phantom type.
data DirOrFile

-- | The phantom type of directories.
foreign import data Dir :: DirOrFile

-- | The phantom type of files.
foreign import data File :: DirOrFile

-- | A class that enables writing operations that abstract over `DirOrFile`.
-- |
-- | The provided `onDirOrFile` function folds over a value indexed by
-- | `DirOrFile` to produce a new result, passing proof/coercion functions to
-- | allow the inner functions to unify their return types if remapping.
class IsDirOrFile :: DirOrFile -> Constraint
class IsDirOrFile b where
  onDirOrFile
    :: forall f r
     . ((f Dir -> f b) -> f Dir -> r)
    -> ((f File -> f b) -> f File -> r)
    -> f b
    -> r

instance isDirOrFileDir :: IsDirOrFile Dir where
  onDirOrFile f _ = f identity

instance isDirOrFileFile :: IsDirOrFile File where
  onDirOrFile _ f = f identity

-- | Folds over a value that uses `DirOrFile` to produce a new result.
foldDirOrFile
  :: forall f b r
   . IsDirOrFile b
  => (f Dir -> r)
  -> (f File -> r)
  -> f b
  -> r
foldDirOrFile f g = onDirOrFile (const f) (const g)
