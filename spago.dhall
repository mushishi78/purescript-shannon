{ name = "shannon"
, dependencies =
  [ "arrays"
  , "dexie"
  , "foreign-object"
  , "maybe"
  , "nonempty"
  , "prelude"
  , "record"
  , "typelevel-peano"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/mushishi78/purescript-shannon.git"
}
