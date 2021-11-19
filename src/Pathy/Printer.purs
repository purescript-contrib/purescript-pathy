module Pathy.Printer
  ( Printer
  , posixPrinter
  , windowsPrinter
  , printPath
  , unsafePrintPath
  , debugPrintPath
  , Escaper(..)
  , slashEscaper
  , dotEscaper
  , posixEscaper
  , windowsEscaper
  , escape
  ) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, un, unwrap)
import Data.String (Pattern(..)) as Str
import Data.String.CodeUnits (singleton) as Str
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty (NonEmptyReplacement(..), replaceAll, toString, unsafeFromString) as NES
import Data.String.NonEmpty.CodeUnits (cons, singleton) as NES
import Partial.Unsafe (unsafePartial)
import Pathy.Name (Name)
import Pathy.Path (Path, foldPath, (</>))
import Pathy.Phantom (class IsDirOrFile, class IsRelOrAbs, Dir, Rel, foldDirOrFile, foldRelOrAbs)
import Pathy.Sandboxed (SandboxedPath, sandboxRoot, unsandbox)
import Prim.TypeError (class Warn, Text)

-- | A `Printer` defines options for printing paths.
-- |
-- | - `root` is a function used to construct the initial segment of paths.
-- | - `current` is a representation of the current directory.
-- | - `up` is a representation of going up to the parent directory.
-- | - `sep` is the string to separate path segments by.
-- | - `escaper` specified how to deal with printing reserved names and
-- |    characters.
type Printer =
  { root :: Maybe NonEmptyString -> String
  , current :: NonEmptyString
  , up :: NonEmptyString
  , sep :: NonEmptyString
  , escaper :: Escaper
  }

-- | A printer for POSIX paths.
posixPrinter :: Printer
posixPrinter =
  { root: maybe "/" (\name -> "/" <> NES.toString (un Escaper posixEscaper name))
  , current: NES.singleton '.'
  , up: NES.singleton '.' <> NES.singleton '.'
  , sep: NES.singleton '/'
  , escaper: posixEscaper
  }

-- | A printer for Windows paths.
windowsPrinter :: Printer
windowsPrinter =
  { root: maybe "\\" (\drive -> NES.toString drive <> ":")
  , current: NES.singleton '.'
  , up: NES.singleton '.' <> NES.singleton '.'
  , sep: NES.singleton '\\'
  , escaper: windowsEscaper
  }

-- | Prints a `SandboxedPath` into its canonical `String` representation, using
-- | the specified printer. The printed path will always be absolute, as this
-- | is the only way to ensure the path is safely referring to the intended
-- | location.
printPath
  :: forall a b
   . IsRelOrAbs a
  => IsDirOrFile b
  => Printer
  -> SandboxedPath a b
  -> String
printPath r sp =
  let
    root = sandboxRoot sp
    p = unsandbox sp
  in
    printPathRep
      r
      (foldRelOrAbs (root </> _) identity p)

-- | Prints a `SandboxedPath` into its canonical `String` representation, using
-- | the specified printer. This will print a relative path if `b ~ Rel`, which
-- | depending on how the resulting string is used, may be unsafe.
unsafePrintPath
  :: forall a b
   . IsRelOrAbs a
  => IsDirOrFile b
  => Printer
  -> SandboxedPath a b
  -> String
unsafePrintPath r sp = printPathRep r (unsandbox sp)

-- | Prints a path exactly according to its representation. This should only be
-- | used for debug purposes. Using this function will raise a warning at
-- | compile time as a reminder!
debugPrintPath
  :: forall a b
   . Warn (Text "debugPrintPath usage")
  => IsRelOrAbs a
  => IsDirOrFile b
  => Printer
  -> Path a b
  -> String
debugPrintPath = printPathRep

printPathRep
  :: forall a b
   . IsRelOrAbs a
  => IsDirOrFile b
  => Printer
  -> Path a b
  -> String
printPathRep printer inputPath = go inputPath
  where
  go :: forall a' b'. IsRelOrAbs a' => IsDirOrFile b' => Path a' b' -> String
  go = foldPath caseCurrent caseParentOf caseIn

  isAbs :: Boolean
  isAbs = foldRelOrAbs (const false) (const true) inputPath

  caseCurrent :: String
  caseCurrent =
    if isAbs then printer.root Nothing
    else NES.toString $ printer.current <> printer.sep

  caseParentOf :: Path Rel Dir -> String
  caseParentOf p = go p <> NES.toString (printer.up <> printer.sep)

  caseIn :: forall a' b'. IsRelOrAbs a' => IsDirOrFile b' => Path a' Dir -> Name b' -> String
  caseIn p name = name # foldDirOrFile
    ( \dirName -> p # foldPath
        ( if isAbs then printer.root (Just $ unwrap dirName) <> NES.toString printer.sep
          else caseCurrent <> printSegment printer dirName <> NES.toString printer.sep
        )
        (\p' -> caseParentOf p' <> printSegment printer dirName <> NES.toString printer.sep)
        (\p' n' -> caseIn p' n' <> printSegment printer dirName <> NES.toString printer.sep)
    )
    (\fileName -> go p <> printSegment printer fileName)

-- | Prints a name as a `String` using the escaper from the specified printer.
printSegment :: forall name. Newtype name NonEmptyString => Printer -> name -> String
printSegment printer = NES.toString <<< un Escaper printer.escaper <<< unwrap

-- | An `Escaper` encodes segments or characters which have reserved meaning
-- | within names in a path.
newtype Escaper = Escaper (NonEmptyString -> NonEmptyString)

derive instance newtypeEscaper :: Newtype Escaper _

instance semigroupEscaper :: Semigroup Escaper where
  append (Escaper e1) (Escaper e2) = Escaper (e1 <<< e2)

instance monoidEscaper :: Monoid Escaper where
  mempty = Escaper identity

-- | An escaper that replaces all `'/'` characters in a name with `'-'`s.
slashEscaper :: Escaper
slashEscaper = Escaper (NES.replaceAll slash dash)
  where
  slash = Str.Pattern "/"
  dash = NES.NonEmptyReplacement (NES.singleton '-')

-- | An escaper that replaces names `"."` and `".."` with `"$dot"` and
-- | `"$dot$dot"`.
dotEscaper :: Escaper
dotEscaper = Escaper \s -> case NES.toString s of
  ".." -> unsafePartial NES.unsafeFromString "$dot$dot"
  "." -> unsafePartial NES.unsafeFromString "$dot"
  _ -> s

-- | An escaper that removes all slashes, converts ".." into "$dot$dot", and
-- | converts "." into "$dot".
posixEscaper :: Escaper
posixEscaper = slashEscaper <> dotEscaper

-- | An escaper that attempts to encode all reserved names and characters for
-- | windows-style paths.
windowsEscaper :: Escaper
windowsEscaper = badCharEscaper <> badNameEscaper <> dotEscaper
  where
  badCharEscaper =
    fold $ map
      (\c -> Escaper (NES.replaceAll (Str.Pattern (Str.singleton c)) dash))
      [ '\\', '/', ':', '*', '?', '"', '<', '>', '|' ]
  badNameEscaper =
    fold $ map
      (\n -> Escaper (NES.replaceAll (Str.Pattern n) (NES.NonEmptyReplacement (NES.cons '$' n))))
      [ "CON", "PRN", "AUX", "NUL", "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9", "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9" ]
  dash = NES.NonEmptyReplacement (NES.singleton '-')

-- | Prints a name as a `String` using the specified escaper.
escape :: forall name. Newtype name NonEmptyString => Escaper -> name -> String
escape r = NES.toString <<< un Escaper r <<< unwrap
