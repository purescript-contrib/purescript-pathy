module Pathy.Name where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.String as S
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty.CodeUnits as NES
import Data.Symbol (class IsSymbol)
import Data.Symbol (reflectSymbol) as Symbol
import Pathy.Phantom (DirOrFile)
import Type.Data.Boolean (False) as Symbol
import Type.Data.Symbol (class Equals) as Symbol
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A type used for both directory and file names, indexed by `DirOrFile`.
newtype Name :: DirOrFile -> Type
newtype Name n = Name NonEmptyString

derive instance newtypeName :: Newtype (Name n) _
derive newtype instance eqName :: Eq (Name a)
derive newtype instance ordName :: Ord (Name a)

instance showName :: Show (Name a) where
  show (Name name) = "(Name " <> show name <> ")"

-- | Splits `Name` in name and extension part.
-- |
-- | ```purescript
-- | splitName (Name ".foo")    == { name: ".foo", extension: Nothing }
-- | splitName (Name "foo.")    == { name: "foo.", extension: Nothing }
-- | splitName (Name "foo")     == { name: "foo",  extension: Nothing }
-- | splitName (Name ".")       == { name: ".",    extension: Nothing }
-- | splitName (Name "foo.baz") == { name: "foo",  extension: Just "baz" }
-- | ```
-- | _Note, in real code all strings from this examples would be `NonEmptyString`._
-- |
-- | Also for any `Name` this property holds:
-- | ```purescript
-- | joinName <<< splitName = id
-- | ````
-- | see [`joinName`](#v:joinName).
splitName :: forall n. Name n -> { name :: NonEmptyString, ext :: Maybe NonEmptyString }
splitName (Name nameIn) =
  fromMaybe { name: nameIn, ext: Nothing } do
    idx <- NES.lastIndexOf (S.Pattern ".") nameIn
    name <- NES.take idx nameIn
    ext <- NES.drop (idx + 1) nameIn
    pure $ { name, ext: Just ext }

-- | Joins name and extension part into one `Name`.
-- |
-- | Also for any `Name` this property holds:
-- | ```purescript
-- | joinName <<< splitName = id
-- | ````
-- | see [`splitName`](#v:splitName).
joinName :: forall n. { name :: NonEmptyString, ext :: Maybe NonEmptyString } -> Name n
joinName { name, ext } = Name $ case ext of
  Nothing -> name
  Just ext' -> name <> NES.singleton '.' <> ext'

-- | Retrieves the extension of a name. also see [`splitName`](#v:splitName)
-- |
-- | ```purescript
-- | extension (Name ".foo")    == Nothing
-- | extension (Name "foo.")    == Nothing
-- | extension (Name ".")       == Nothing
-- | extension (Name "foo.baz") == Just "baz"
-- | ````
-- | _Note, in real code all strings from this examples would be `NonEmptyString`._
extension :: forall n. Name n -> Maybe NonEmptyString
extension = splitName >>> _.ext

-- | Alters an extension of a name. This allows extensions to be added, removed,
-- | or modified. see [`splitName`](#v:splitName) and [`joinName`](#v:joinName)
-- | for how a `Name` is split into name and extention part and joined back
-- | into a `Name`.
-- |
-- | Also for any `Name` this property holds:
-- | ```purescript
-- | alterExtension id = id
-- | ````
alterExtension
  :: forall n
   . (Maybe NonEmptyString -> Maybe NonEmptyString)
  -> Name n
  -> Name n
alterExtension f n = do
  let spn = splitName n
  joinName spn { ext = f spn.ext }

-- | A class for creating `Name` values from type-level strings. This allows us
-- | to guarantee that a name is not empty at compile-time.
class IsName :: Symbol -> Constraint
class IsName sym where
  reflectName :: forall proxy d. proxy sym -> Name d

instance isNameNESymbol :: (IsSymbol s, Symbol.Equals s "" Symbol.False) => IsName s where
  reflectName _ = asNonEmpty $ Symbol.reflectSymbol (Proxy :: Proxy s)
    where
    asNonEmpty :: forall d. String -> Name d
    asNonEmpty = unsafeCoerce
