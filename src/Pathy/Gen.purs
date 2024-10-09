module Pathy.Gen
  ( genAbsDirPath
  , genAbsFilePath
  , genRelDirPath
  , genRelFilePath
  , genAnyDirPathVariant
  , genAnyFilePathVariant
  , genAbsAnyPathVariant
  , genRelAnyPathVariant
  , genAnyAnyPathVariant
  , genName
  , genDirName
  , genFileName
  ) where

import Pathy.Path (AbsAnyPathVariant, AbsDir, AbsFile, AnyAnyPathVariant, AnyDirPathVariant, AnyFilePathVariant, RelAnyPathVariant, RelDir, RelFile, proxyAbsDir, proxyAbsFile, proxyRelDir, proxyRelFile, (</>))
import Pathy.Phantom (Dir, File)
import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Char.Gen as CG
import Data.Foldable (foldr)
import Data.List as L
import Data.NonEmpty ((:|))
import Data.String.Gen as SG
import Data.String.NonEmpty.CodeUnits (cons)
import Data.Variant (inj)
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

genAnyDirPathVariant :: forall m. MonadGen m => MonadRec m => m AnyDirPathVariant
genAnyDirPathVariant = Gen.oneOf $ (inj proxyRelDir <$> genRelDirPath) :| [ inj proxyAbsDir <$> genAbsDirPath ]

genAnyFilePathVariant :: forall m. MonadGen m => MonadRec m => m AnyFilePathVariant
genAnyFilePathVariant = Gen.oneOf $ (inj proxyRelFile <$> genRelFilePath) :| [ inj proxyAbsFile <$> genAbsFilePath ]

genRelAnyPathVariant :: forall m. MonadGen m => MonadRec m => m RelAnyPathVariant
genRelAnyPathVariant = Gen.oneOf $ (inj proxyRelDir <$> genRelDirPath) :| [ inj proxyRelFile <$> genRelFilePath ]

genAbsAnyPathVariant :: forall m. MonadGen m => MonadRec m => m AbsAnyPathVariant
genAbsAnyPathVariant = Gen.oneOf $ (inj proxyAbsDir <$> genAbsDirPath) :| [ inj proxyAbsFile <$> genAbsFilePath ]

genAnyAnyPathVariant :: forall m. MonadGen m => MonadRec m => m AnyAnyPathVariant
genAnyAnyPathVariant = Gen.oneOf $
  (inj proxyRelDir <$> genRelDirPath) :|
    [ inj proxyAbsDir <$> genAbsDirPath
    , inj proxyRelFile <$> genRelFilePath
    , inj proxyAbsFile <$> genAbsFilePath
    ]
