module Beecarbonize.Decode exposing (card, event, languages)

import Beecarbonize exposing (..)

import Bytes exposing (..)
import Bytes.Decode exposing (..)
import Dict exposing (Dict)
import Hex.Convert as Hex

cardAssembler : Identification -> Building -> Production -> Events -> Misc -> Card
cardAssembler id build prod ev mc =
  { objectName = id.objectName
  , id = id.id
  , displayName = id.displayName
  , buildCost = build.buildCost
  , dubiousValue = build.dubiousValue
  , rebate = build.rebate
  , instantEmissions = build.instantEmissions
  , buildableCardsIds = build.buildableCardsIds
  , buildTime = prod.buildTime
  , speed = prod.speed
  , emissions = prod.emissions
  , sector = prod.sector
  , replacedByBuild = ev.replacedByBuild
  , randomBuild = ev.randomBuild
  , cardType = ev.cardType
  , eventChances = ev.eventChances
  , eventsOnDestruction = ev.eventsOnDestruction
  , spriteId = mc.spriteId
  , arbitraryValue = mc.arbitraryValue
  , internalName = mc.internalName
  }

card : Decoder Card
card =
  map5 cardAssembler
    identification
    building
    production
    events
    misc

eventAssembler : Identification -> EventStuff -> EventEffects -> Activation -> Trailer -> Event
eventAssembler id es ef act trail =
  { objectName = id.objectName
  , id = id.id
  , displayName = id.displayName
  , duration = es.duration
  , eventType = es.eventType
  , v2 = es.v2
  , v3 = es.v3
  , everyRoundEffect = ef.everyRoundEffect
  , expireEffect = ef.expireEffect
  , solveEffect = ef.solveEffect
  , insolvencyEffect = ef.insolvencyEffect
  , solutionCost = act.solutionCost
  , tippingPoint = act.tippingPoint
  , emissionRange = act.emissionRange
  , probability = act.probability
  , minRounds = act.minRounds
  , v5 = trail.v5
  , spriteId = trail.spriteId
  }

event : Decoder Event
event =
  map5 eventAssembler
    identification
    eventStuff
    eventEffects
    activation
    trailer

languages : Decoder (Dict String String)
languages =
  discard 56
    <| translationList

type alias Identification =
  { class : ObjectId
  , objectName : String
  , id : Int
  , displayName : String
  }

identification : Decoder Identification
identification =
  map4 Identification
    (expect "before class" [0,0,0,1] objectId)
    (expect "before name" [0] prefixedString)
    cardId
    prefixedString
    |> map (log "id")

type alias Building =
  { buildCost : Tokens
  , dubiousValue : Int
  , rebate : Tokens
  , instantEmissions : Int
  , buildableCardsIds : List Int
  }

building : Decoder Building
building =
  map5 Building
    tokens
    arbitraryValue
    tokens
    emissions
    listOfIds
    |> map (log "build")

type alias Production =
  { buildTime : Int
  , speed : Float
  , emissions : Int
  , sector : Sector
  , arbitraryList : List Int
  }

production : Decoder Production
production =
  map5 Production
    buildTime
    speed
    emissions
    sector
    arbitraryList
    |> map (log "production")

type alias Events =
  { replacedByBuild : Bool
  , randomBuild : Bool
  , cardType : CardType
  , eventChances : List EventChance
  , eventsOnDestruction : List Int
  }

events : Decoder Events
events =
  map5 Events
    replacedByBuild
    randomBuild
    cardType
    eventChances
    listOfIds
    |> map (log "events")

type alias Misc =
  { spriteId : ObjectId
  , arbitraryValue : Int
  , internalName : String
  }

misc : Decoder Misc
misc =
  map3 Misc
    spriteId
    (expect "before arbitrary" [0] arbitraryValue)
    prefixedString
    |> map (log "misc")

type alias EventStuff =
  { eventType : EventType
  , v2 : Int
  , duration : Int
  , v3 : Int
  }

eventStuff : Decoder EventStuff
eventStuff =
  map4 EventStuff
    eventType
    arbitraryValue
    buildTime
    arbitraryValue
    |> map (log "eventStuff")

type alias EventEffects =
  { everyRoundEffect : Effect
  , expireEffect : Effect
  , solveEffect : Effect
  , insolvencyEffect : Effect
  }

eventEffects : Decoder EventEffects
eventEffects =
  map4 EventEffects
    effect
    effect
    effect
    effect
    |> map (log "effects")

type alias Activation =
  { solutionCost : Tokens
  , tippingPoint : TippingPoint
  , emissionRange : (Int,Int)
  , probability : Float
  , minRounds : Int
  }

activation : Decoder Activation
activation =
  map5 Activation
    tokens
    tippingPoint
    emissionRange
    (expect "before probability" [0] probability)
    rounds
    |> map (log "activation")

type alias Trailer =
  { v5 : Int
  , spriteId : ObjectId
  , internalName : String
  }

trailer : Decoder Trailer
trailer =
  map3 Trailer
    arbitraryValue
    spriteId
    prefixedString
    |> map (log "trailer")

tippingPoint : Decoder TippingPoint
tippingPoint =
  unsignedInt32 LE
    |> map (\x -> if x == 0 then Random else TippingPoint x)

emissionRange : Decoder (Int, Int)
emissionRange =
  map2 Tuple.pair
    (unsignedInt32 LE)
    (unsignedInt32 LE)

effect : Decoder Effect
effect =
  map4 Effect
    effectType
    tokens
    listOfIds
    emissions
    |> map (log "effect")

prefixedString : Decoder String
prefixedString =
  unsignedInt32 LE
    |> andThen paddedString

paddedString : Int -> Decoder String
paddedString length =
  map2 (\x _ -> x)
    (string length)
    (bytes (modBy 4 -length))

cardId : Decoder Int
cardId =
  unsignedInt32 LE

spriteId : Decoder ObjectId
spriteId =
  objectId

objectId : Decoder ObjectId
objectId =
  map2 ObjectId
    (unsignedInt32 LE)
    (unsignedInt32 LE)

arbitraryValue : Decoder Int
arbitraryValue =
  unsignedInt32 LE

buildTime : Decoder Int
buildTime =
  unsignedInt32 LE

rounds : Decoder Int
rounds =
  unsignedInt32 LE

speed : Decoder Float
speed =
  float32 LE

emissions : Decoder Int
emissions =
  signedInt32 LE

sector : Decoder Sector
sector =
  unsignedInt32 LE
    |> andThen (\x -> case x of
        1 -> succeed Industry
        2 -> succeed People
        3 -> succeed Environment
        4 -> succeed Science
        _ -> failLog "unknown sector" x
      )

effectType : Decoder EffectType
effectType =
  unsignedInt32 LE
    |> andThen (\x -> case x of
        0 -> succeed NoEffect
        1 -> succeed GainResources
        2 -> succeed LoseResources
        3 -> succeed DestroyCards
        4 -> succeed CreateCards
        5 -> succeed ModifyEmissions
        6 -> succeed LoseGame
        _ -> failLog "unknown effect type" x
      )

replacedByBuild = bool
randomBuild = bool

bool : Decoder Bool
bool =
  unsignedInt32 LE
    |> andThen (\x -> case x of
      0 -> succeed False
      1 -> succeed True
      _ -> failLog "unknown boolean value" x
    )

cardType : Decoder CardType
cardType =
  unsignedInt32 LE
    |> andThen (\x -> case x of
        0 -> succeed Dubious
        1 -> succeed Normal
        2 -> succeed SomethingElse
        4 -> succeed Starred
        5 -> succeed Victory
        _ -> failLog "unknown card type" x
      )

eventType : Decoder EventType
eventType =
  unsignedInt32 LE
    |> andThen (\x -> case x of
        1 -> succeed Negative
        2 -> succeed Positive
        _ -> failLog "unknown event type" x
      )

probability : Decoder Float
probability =
  float32 LE

eventChances : Decoder (List EventChance)
eventChances =
  list (eventChance)
    |> map (log "chances")

eventChance : Decoder EventChance
eventChance =
  map2 EventChance
    cardId
    probability

tokens : Decoder Tokens
tokens =
  map3 Tokens
    (unsignedInt32 LE)
    (unsignedInt32 LE)
    (unsignedInt32 LE)
    |> map (log "tokens")

listOfIds : Decoder (List Int)
listOfIds =
  list (unsignedInt32 LE)
    --|> map (log "ids")

arbitraryList : Decoder (List Int)
arbitraryList =
  list (unsignedInt32 LE)
    |> map (log "arbitrary")

listOfStrings : Decoder (List String)
listOfStrings =
  list prefixedString
    --|> map (log "strings")

type alias LanguageEntry =
  { key : String
  , v1 : Int
  , translations : List String
  , mystery : List Int
  }

languageEntry : Decoder LanguageEntry
languageEntry =
  map4 LanguageEntry
    prefixedString
    arbitraryValue
    listOfStrings
    listOfIds
    --|> map (log "langentry")

langToPair : LanguageEntry -> (String, String)
langToPair lang =
  (lang.key, lang.translations |> List.head |> Maybe.withDefault lang.key)

languagePair : Decoder (String, String)
languagePair =
  languageEntry
    |> map langToPair

translationList : Decoder (Dict String String)
translationList =
  list languagePair
    --|> map (log "trans")
    |> map Dict.fromList

list : Decoder a -> Decoder (List a)
list decoder =
  unsignedInt32 LE
    --|> map (log "n")
    |> andThen (\len -> loop (len, []) (listStep decoder))

listStep : Decoder a -> (Int, List a) -> Decoder (Step (Int, List a) (List a))
listStep decoder (n, xs) =
  if n <= 0 then
    succeed (Done (List.reverse xs))
  else
    map (\x -> Loop (n - 1, x :: xs)) decoder

discard : Int -> Decoder a -> Decoder a
discard count x =
  bytes count
    |> map ( Hex.toString >> log "discard")
    |> andThen (always x)

discardInts : Int -> Decoder a -> Decoder a
discardInts count x =
  loop (count//4, []) (listStep (unsignedInt32 LE))
    |> map (log "discard")
    |> andThen (always x)

expect : String -> List Int -> Decoder a -> Decoder a
expect label expected x =
  loop (List.length expected, []) (listStep (unsignedInt32 LE))
    |> andThen (compare label x expected)

compare : String -> Decoder a -> List Int -> List Int -> Decoder a
compare label next expected actual =
  case expected of
    e :: ez ->
      case actual of
        a :: az ->
          if e /= a then
            failLog label (expected, actual)
          else
            compare label next ez az
        [] ->
          failLog label (expected, actual)
    [] -> 
      case actual of
        a :: az ->
          failLog label (expected, actual)
        [] ->
          next

toList : Bytes -> Maybe (List Int)
toList b =
  decode (loop (width b, []) (listStep unsignedInt8)) b

dump title x =
  let _ = log title (Hex.toString x) in x

log title x =
  --Debug.log title x
  x

failLog title x =
  --let _ = Debug.log title x in fail
  fail
