module Data.Path.Pathy.Gen
  ( genAbsDirPath
  , genAbsFilePath
  , genAbsAnyPath
  , genRelDirPath
  , genRelFilePath
  , genRelAnyPath
  )where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Char.Gen as CG
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List as L
import Data.NonEmpty ((:|))
import Data.Path.Pathy (AbsPath, AbsFile, AbsDir, RelDir, RelFile, RelPath, (</>))
import Data.Path.Pathy as P
import Data.String.Gen as SG
import Data.String.NonEmpty (NonEmptyString, cons)

genName ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m NonEmptyString
genName = cons <$> genChar <*> SG.genString genChar
  where
  genChar = Gen.oneOf $ CG.genDigitChar :| [CG.genAlpha]


genAbsDirPath :: forall m. MonadGen m => MonadRec m => m AbsDir
genAbsDirPath = Gen.sized \size → do
  newSize ← Gen.chooseInt 0 size
  Gen.resize (const newSize) do
    parts ∷ L.List NonEmptyString ← Gen.unfoldable genName
    pure $ foldr (flip P.appendPath <<< P.dir) P.rootDir parts

genAbsFilePath :: forall m. MonadGen m => MonadRec m => m AbsFile
genAbsFilePath = do
  dir ← genAbsDirPath
  file ← genName
  pure $ dir </> P.file file

genAbsAnyPath :: forall m. MonadGen m => MonadRec m => m AbsPath
genAbsAnyPath = Gen.oneOf $ (Left <$> genAbsDirPath) :| [Right <$> genAbsFilePath]

genRelDirPath :: forall m. MonadGen m => MonadRec m => m RelDir
genRelDirPath = Gen.sized \size → do
  newSize ← Gen.chooseInt 0 size
  Gen.resize (const newSize) do
    parts ∷ L.List NonEmptyString ← Gen.unfoldable genName
    pure $ foldr (flip P.appendPath <<< P.dir) P.currentDir parts

genRelFilePath :: forall m. MonadGen m => MonadRec m => m RelFile
genRelFilePath = do
  dir ← genRelDirPath
  file ← genName
  pure $ dir </> P.file file

genRelAnyPath :: forall m. MonadGen m => MonadRec m => m RelPath
genRelAnyPath = Gen.oneOf $ (Left <$> genRelDirPath) :| [Right <$> genRelFilePath]
