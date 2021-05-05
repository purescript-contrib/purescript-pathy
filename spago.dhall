{ name = "pathy"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "gen"
  , "identity"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "partial"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "quickcheck"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
