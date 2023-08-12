module Beecarbonize exposing
  ( Card
  , Event
  , Effect
  , Tokens(..)
  , EventChance(..)
  , CardType(..)
  , Sector(..)
  , EffectType(..)
  , TippingPoint(..)
  , EventType(..)
  , noEffect
  , loseResources
  , gainResources
  , destroyCards
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

type alias Event =
  { objectName : String
  , id : Int
  , displayName : String
  , duration : Int
  , eventType : EventType
  , v2 : Int
  , v3 : Int
  , everyRoundEffect : Effect
  , expireEffect : Effect
  , solveEffect : Effect
  , insolvencyEffect : Effect
  , tippingPoint : TippingPoint
  , solutionCost : Tokens
  , emissionRange : (Int, Int)
  , probability : Float
  , minRounds : Int
  , v5 : Int
  , spriteId : Int
  }

type EventType
  = Negative
  | Positive

type TippingPoint
  = Random
  | TippingPoint Int

type EffectType
  = NoEffect
  | GainResources
  | LoseResources
  | DestroyCards
  | CreateCards
  | ModifyEmissions
  | LoseGame

type alias Effect =
  { effectType : EffectType
  , tokens : Tokens
  , targetIds : List Int
  , emissions : Int
  }

noEffect : Effect
noEffect =
  { effectType = NoEffect
  , tokens = Tokens 0 0 0
  , targetIds = []
  , emissions = 0
  }

loseResources : Tokens -> Effect
loseResources tokens =
  { effectType = LoseResources
  , tokens = tokens
  , targetIds = []
  , emissions = 0
  }

gainResources : Tokens -> Effect
gainResources tokens =
  { effectType = GainResources
  , tokens = tokens
  , targetIds = []
  , emissions = 0
  }

destroyCards : List Int -> Effect
destroyCards cards =
  { effectType = DestroyCards
  , tokens = Tokens 0 0 0
  , targetIds = cards
  , emissions = 0
  }
