module Data.Path.Pathy.Parser
  ( Parser(..)
  , posixParser
  , parsePath
  , parseRelFile
  , parseAbsFile
  , parseRelDir
  , parseAbsDir
  ) where

import Prelude

import Data.Array as A
import Data.Either (Either(..), either)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (AbsDir, AbsFile, Path, RelDir, RelFile, currentDir, extendPath, parentOf, rootDir)
import Data.Path.Pathy.Name (Name(..))
import Data.Path.Pathy.Phantom (Dir)
import Data.String as S
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES

newtype Parser = Parser (forall z. (RelDir -> z) -> (AbsDir -> z) -> (RelFile -> z) -> (AbsFile -> z) -> z -> String -> z)

-- | A parser for POSIX paths.
posixParser :: Parser
posixParser = Parser \relDir absDir relFile absFile z ->
  case _ of
    "" -> z
    "/" -> absDir rootDir
    p ->
      let
        isAbs = S.take 1 p == "/"
        isFile = S.takeRight 1 p /= "/"
        segs = L.fromFoldable $ A.reverse $ A.mapMaybe NES.fromString $ S.split (S.Pattern "/") p
      in
        case isAbs, isFile of
          true, true -> buildPath z rootDir (either (const z) absFile) segs
          true, false -> buildPath z rootDir (either absDir absDir) segs
          false, true -> buildPath z currentDir (either (const z) relFile) segs
          false, false -> buildPath z currentDir (either relDir relDir) segs

buildPath
  :: forall z a b
  . z
  -> Path a Dir
  -> (Either (Path a Dir) (Path a b) -> z)
  -> List NonEmptyString
  -> z
buildPath z init k segs =
  case segs of
    Nil -> z
    name : segs'
      | NES.toString name == ".." -> k $ Left (parentOf (go segs'))
      | NES.toString name == "." -> k $ Left (go segs')
      | otherwise -> k $ Right (extendPath (go segs') (Name name))
  where
    go :: List NonEmptyString -> Path a Dir
    go = case _ of
      Nil -> init
      name : segs'
        | NES.toString name == ".." -> parentOf (go segs')
        | NES.toString name == "." -> go segs'
        | otherwise -> extendPath (go segs') (Name name)

parsePath
  :: forall z
   . Parser
  -> (RelDir -> z)
  -> (AbsDir -> z)
  -> (RelFile -> z)
  -> (AbsFile -> z)
  -> z
  -> String
  -> z
parsePath (Parser p) = p

-- | Attempts to parse a relative file.
parseRelFile :: Parser -> String -> Maybe RelFile
parseRelFile p = parsePath p (const Nothing) (const Nothing) Just (const Nothing) Nothing

-- | Attempts to parse an absolute file.
parseAbsFile :: Parser -> String -> Maybe AbsFile
parseAbsFile p = parsePath p (const Nothing) (const Nothing) (const Nothing) Just Nothing

-- | Attempts to parse a relative directory.
parseRelDir :: Parser -> String -> Maybe RelDir
parseRelDir p = parsePath p Just (const Nothing) (const Nothing) (const Nothing) Nothing

-- | Attempts to parse an absolute directory.
parseAbsDir :: Parser -> String -> Maybe AbsDir
parseAbsDir p = parsePath p (const Nothing) Just (const Nothing) (const Nothing) Nothing
