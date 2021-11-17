module Pathy.Gen
  ( genAbsDirPath
  , genAbsFilePath
  , genAbsAnyPath
  , genRelDirPath
  , genRelFilePath
  , genRelAnyPath
  , genName
  , genDirName
  , genFileName
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Char.Gen as CG
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List as L
import Data.NonEmpty ((:|))
import Data.String.Gen as SG
import Data.String.NonEmpty.CodeUnits (cons)
import Pathy (AbsDir, AbsFile, AbsPath, Dir, File, RelDir, RelFile, RelPath, (</>))
import Pathy as P

genName :: forall m a. MonadGen m => MonadRec m => m (P.Name a)
genName = map P.Name $ cons <$> genChar <*> SG.genString genChar
  where
  genChar = Gen.oneOf $ CG.genDigitChar :| [ CG.genAlpha ]

genDirName :: forall m. MonadGen m => MonadRec m => m (P.Name Dir)
genDirName = genName

genFileName :: forall m. MonadGen m => MonadRec m => m (P.Name File)
genFileName = genName

genAbsDirPath :: forall m. MonadGen m => MonadRec m => m AbsDir
genAbsDirPath = Gen.sized \size -> do
  newSize <- Gen.chooseInt 0 size
  Gen.resize (const newSize) do
    parts :: L.List (P.Name Dir) <- Gen.unfoldable genName
    pure $ foldr (flip P.appendPath <<< P.dir') P.rootDir parts

genAbsFilePath :: forall m. MonadGen m => MonadRec m => m AbsFile
genAbsFilePath = do
  dir <- genAbsDirPath
  file <- genName
  pure $ dir </> P.file' file

genAbsAnyPath :: forall m. MonadGen m => MonadRec m => m AbsPath
genAbsAnyPath = Gen.oneOf $ (Left <$> genAbsDirPath) :| [ Right <$> genAbsFilePath ]

genRelDirPath :: forall m. MonadGen m => MonadRec m => m RelDir
genRelDirPath = Gen.sized \size -> do
  newSize <- Gen.chooseInt 0 size
  Gen.resize (const newSize) do
    parts :: L.List (P.Name Dir) <- Gen.unfoldable genName
    pure $ foldr (flip P.appendPath <<< P.dir') P.currentDir parts

genRelFilePath :: forall m. MonadGen m => MonadRec m => m RelFile
genRelFilePath = do
  dir <- genRelDirPath
  file <- genName
  pure $ dir </> P.file' file

genRelAnyPath :: forall m. MonadGen m => MonadRec m => m RelPath
genRelAnyPath = Gen.oneOf $ (Left <$> genRelDirPath) :| [ Right <$> genRelFilePath ]
