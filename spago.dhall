{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [  
    "arrays", 
    "bifunctors",
    "console",
    "control",
    "contravariant",
    "effect",
    "either",
    "foldable-traversable",
    "identity",
    "integers",
    "lists",
    "maybe",    
    "newtype",
    "nonempty",
    "prelude",
    "profunctor",
    "strings",
    "psci-support",
    "tuples",
    "transformers",
    "unfoldable",
    "undefined",
    "unicode"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
