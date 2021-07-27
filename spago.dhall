{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "tscompat"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "lists"
  , "nullable"
  , "prelude"
  , "react"
  , "strings"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
