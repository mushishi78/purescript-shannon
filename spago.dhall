{ name = "shannon"
, dependencies =
  [ "aff"
  , "arrays"
  , "dexie"
  , "foreign-object"
  , "maybe"
  , "nonempty"
  , "prelude"
  , "record"
  , "transformers"
  , "typelevel-peano"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/mushishi78/purescript-shannon.git"
}
