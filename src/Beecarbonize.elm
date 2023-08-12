module Beecarbonize exposing
  ( Card
  , Tokens(..)
  , EventChance(..)
  , CardType(..)
  , Sector(..)
  )

type Tokens = Tokens Int Int Int

type EventChance = EventChance Int Float

type CardType
  = Dubious
  | Normal
  | SomethingElse
  | Starred
  | Victory

type Sector
  = Industry
  | People
  | Environment
  | Science

type alias Card =
  { objectName : String
  , id : Int
  , displayName : String
  , buildCost : Tokens
  , dubiousValue : Int
  , rebate : Tokens
  , instantEmissions : Int
  , buildableCardsIds : List Int
  , buildTime : Int
  , speed : Float
  , emissions : Int
  , sector : Sector
  , replacedByBuild : Bool
  , randomBuild : Bool
  , cardType : CardType
  , eventChances : List EventChance
  , eventsOnDestruction : List Int
  , spriteId : Int
  , arbitraryValue : Int
  , internalName : String
  }
