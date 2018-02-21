module Pathy.Name where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.String as S
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Pathy.Phantom (kind DirOrFile)

-- | A type used for both directory and file names, indexed by `DirOrFile`.
newtype Name (n :: DirOrFile) = Name NonEmptyString

derive instance newtypeName :: Newtype (Name n) _
derive newtype instance eqName :: Eq (Name a)
derive newtype instance ordName :: Ord (Name a)

instance showName :: Show (Name a) where
  show (Name name) = "(Name " <> show name <> ")"

-- | Retrieves the extension of a name.
extension :: forall n. Name n -> Maybe NonEmptyString
extension (Name name) =
  flip NES.drop name <<< (_ + 1) =<< NES.lastIndexOf (S.Pattern ".") name

-- | Alters an extension of a name. This allows extensions to be added, removed,
-- | or modified.
alterExtension
  :: forall n
   . (Maybe NonEmptyString -> Maybe NonEmptyString)
  -> Name n
  -> Name n
alterExtension f (Name name) =
  case NES.lastIndexOf (S.Pattern ".") name of
    Nothing -> extend name Nothing
    Just i ->
      case NES.splitAt i name of
        Just { before: Just n, after } -> extend n (NES.drop 1 =<< after)
        _ -> extend name Nothing
  where
    extend name' ext =
      maybe
        (Name name')
        (\ext' -> Name (name' <> NES.singleton '.' <> ext'))
        (f ext)
