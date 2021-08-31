{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-grid-game-template"
, dependencies =
  [ "canvas"
  , "colors"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
