{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ 
    "console",
    "contravariant",
    "effect",
    "either",
    "foldable-traversable",
    "integers",
    "lists",
    "maybe",
    "newtype",
    "nonempty",
    "prelude",
    "profunctor",
    "strings",
    "psci-support",
    "tuples"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
