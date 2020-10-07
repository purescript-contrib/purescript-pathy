{ name = "pathy"
, dependencies =
  [ "console"
  , "effect"
  , "exceptions"
  , "lists"
  , "partial"
  , "profunctor"
  , "psci-support"
  , "quickcheck"
  , "strings"
  , "transformers"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
