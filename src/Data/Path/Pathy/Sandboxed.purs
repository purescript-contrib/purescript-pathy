module Data.Path.Pathy.Sandboxed
  ( SandboxedPath
  -- , sandbox
  -- , sandboxAny
  , sandboxRoot
  , unsandbox
  , printPath
  , printPath'
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path.Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, Path, Printer, Rel, canonicalize, onRelOrAbs, posixPrinter, relativeTo, rootDir, unsafePrintPath', (</>))

-- | The type for paths that have been sandboxed.
data SandboxedPath a b = SandboxedPath (Path Abs Dir) (Path a b)

derive instance eqSandboxedPath :: (IsRelOrAbs a, IsDirOrFile b) => Eq (SandboxedPath a b)
derive instance ordSandboxedPath :: (IsRelOrAbs a, IsDirOrFile b) => Ord (SandboxedPath a b)
instance showSandboxedPath :: (IsRelOrAbs a, IsDirOrFile b) => Show (SandboxedPath a b) where
  show (SandboxedPath root path) = "(SandboxedPath " <> show root <> " " <> show path <> ")"

-- | Attempts to sandbox a path relative to an absolute directory ("sandbox
-- | root"). If the `Path a b` escapes the sandbox root `Nothing` will be
-- | returned.
-- sandbox
--   :: forall a b
--    . IsRelOrAbs a
--   => IsDirOrFile b
--   => Path Abs Dir
--   -> Path a b
--   -> Maybe (SandboxedPath a b)
-- sandbox root = onRelOrAbs goRel goAbs
--   where
--     goRel :: (Path Rel b -> Path a b) -> Path Rel b -> Maybe (SandboxedPath a b)
--     goRel coe p =
--       case (root </> p) `relativeTo` root of
--         Nothing -> Nothing
--         Just _ -> Just (SandboxedPath root (coe p))
--     goAbs :: (Path Abs b -> Path a b) -> Path Abs b -> Maybe (SandboxedPath a b)
--     goAbs coe p =
--       case p `relativeTo` root of
--         Nothing -> Nothing
--         Just _ -> Just (SandboxedPath root (coe p))

-- | Sandboxes any path (a to `/`.
-- |
-- | This should only be used for situations where a path is already constrained
-- | within a system so that access to `/` is safe - for instance, in URIs.
-- sandboxAny
--   :: forall a b
--    . IsRelOrAbs a
--   => IsDirOrFile b
--   => Path a b
--   -> SandboxedPath a b
-- sandboxAny p =
--   fromMaybe (SandboxedPath rootDir (canonicalize p)) (sandbox rootDir p)

-- | Returns the location a `SandboxedPath` was sandboxed to.
sandboxRoot :: forall a b. SandboxedPath a b -> Path Abs Dir
sandboxRoot (SandboxedPath root _) = root

-- | Extracts the original path from a `SandboxedPath`.
unsandbox :: forall a b. SandboxedPath a b -> Path a b
unsandbox (SandboxedPath _ p) = p

-- | Prints a `SandboxedPath` into its canonical `String` representation. The
-- | printed path will always be absolute, as this is the only way to ensure
-- | the path is safely referring to the intended location.
printPath
  :: forall a b
   . IsRelOrAbs a
  => IsDirOrFile b
  => SandboxedPath a b
  -> String
printPath = printPath' posixPrinter

-- | Prints a `SandboxedPath` into its canonical `String` representation, using
-- | the specified printer. The printed path will always be absolute, as this
-- | is the only way to ensure the path is safely referring to the intended
-- | location.
printPath'
  :: forall a b
   . IsRelOrAbs a
  => IsDirOrFile b
  => Printer
  -> SandboxedPath a b
  -> String
printPath' r (SandboxedPath root p) =
  unsafePrintPath'
    r
    (onRelOrAbs (\_ p' -> canonicalize (root </> p')) (flip const) p)
