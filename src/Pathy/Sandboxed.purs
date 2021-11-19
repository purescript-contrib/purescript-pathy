module Pathy.Sandboxed
  ( SandboxedPath
  , sandbox
  , sandboxAny
  , sandboxRoot
  , unsandbox
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Pathy.Path (Path, foldPath, relativeTo, rootDir, (</>))
import Pathy.Phantom (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, onRelOrAbs)

-- | The type for paths that have been sandboxed.
data SandboxedPath a b = SandboxedPath (Path Abs Dir) (Path a b)

derive instance eqSandboxedPath :: (IsRelOrAbs a, IsDirOrFile b) => Eq (SandboxedPath a b)
derive instance ordSandboxedPath :: (IsRelOrAbs a, IsDirOrFile b) => Ord (SandboxedPath a b)
instance showSandboxedPath :: (IsRelOrAbs a, IsDirOrFile b) => Show (SandboxedPath a b) where
  show (SandboxedPath root path) = "(SandboxedPath " <> show root <> " " <> show path <> ")"

-- | Attempts to sandbox a path relative to an absolute directory ("sandbox
-- | root"). If the `Path a b` escapes the sandbox root `Nothing` will be
-- | returned.
sandbox
  :: forall a b
   . IsRelOrAbs a
  => Path Abs Dir
  -> Path a b
  -> Maybe (SandboxedPath a b)
sandbox root = map (SandboxedPath root) <<< onRelOrAbs (go (root </> _)) (go identity)
  where
  go :: forall p. (p -> Path Abs b) -> (p -> Path a b) -> p -> Maybe (Path a b)
  go f coe p =
    if goesUp (f p `relativeTo` root) then Nothing
    else Just (coe p)

  goesUp :: forall x y. Path x y -> Boolean
  goesUp = foldPath false (const true) (\p _ -> goesUp p)

-- | Sandboxes any path to `/`.
-- |
-- | This should only be used for situations where a path is already constrained
-- | within a system so that access to `/` is safe - for instance, in URIs.
sandboxAny :: forall a b. Path a b -> SandboxedPath a b
sandboxAny p = SandboxedPath rootDir p

-- | Returns the location a `SandboxedPath` was sandboxed to.
sandboxRoot :: forall a b. SandboxedPath a b -> Path Abs Dir
sandboxRoot (SandboxedPath root _) = root

-- | Extracts the original path from a `SandboxedPath`.
unsandbox :: forall a b. SandboxedPath a b -> Path a b
unsandbox (SandboxedPath _ p) = p
