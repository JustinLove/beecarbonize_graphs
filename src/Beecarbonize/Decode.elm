module Beecarbonize.Decode exposing (card)

import Beecarbonize exposing (..)

import Bytes exposing (..)
import Bytes.Decode exposing (..)
import Hex.Convert as Hex

assembler : Identification -> Building -> Production -> Events -> Misc -> Card
assembler id build prod ev mc =
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
  map5 assembler
    (expect "before id" [0,0,0,1,1,637,0] identification)
    building
    production
    events
    misc

type alias Identification =
  { objectName : String
  , id : Int
  , displayName : String
  }

identification : Decoder Identification
identification =
  map3 Identification
    prefixedString
    cardId
    prefixedString
    |> map (Debug.log "id")

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
    |> map (Debug.log "build")

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
    |> map (Debug.log "production")

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
    |> map (Debug.log "events")

type alias Misc =
  { spriteId : Int
  , arbitraryValue : Int
  , internalName : String
  }

misc : Decoder Misc
misc =
  map3 Misc
    (expect "before spriteId" [0] spriteId)
    (expect "before arbitrary" [0] arbitraryValue)
    prefixedString
    |> map (Debug.log "misc")

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

spriteId : Decoder Int
spriteId =
  unsignedInt32 LE

arbitraryValue : Decoder Int
arbitraryValue =
  unsignedInt32 LE

buildTime : Decoder Int
buildTime =
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
        _ -> let _ = Debug.log "unknown sector" x in fail
      )

replacedByBuild = bool
randomBuild = bool

bool : Decoder Bool
bool =
  unsignedInt32 LE
    |> andThen (\x -> case x of
      0 -> succeed False
      1 -> succeed True
      _ -> let _ = Debug.log "unknown boolean value" x in fail
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
        _ -> let _ = Debug.log "unknown card type" x in fail
      )

probability : Decoder Float
probability =
  float32 LE

eventChances : Decoder (List EventChance)
eventChances =
  list (eventChance)
    |> map (Debug.log "chances")

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
    |> map (Debug.log "tokens")

listOfIds : Decoder (List Int)
listOfIds =
  list (unsignedInt32 LE)
    |> map (Debug.log "ids")

arbitraryList : Decoder (List Int)
arbitraryList =
  list (unsignedInt32 LE)
    |> map (Debug.log "arbitrary")

list : Decoder a -> Decoder (List a)
list decoder =
  unsignedInt32 LE
    |> map (Debug.log "n")
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
    |> map ( Hex.toString >> Debug.log "discard")
    |> andThen (always x)

discardInts : Int -> Decoder a -> Decoder a
discardInts count x =
  loop (count//4, []) (listStep (unsignedInt32 LE))
    |> map (Debug.log "discard")
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
            let _ = Debug.log label (expected, actual) in
            fail
          else
            compare label next ez az
        [] ->
          let _ = Debug.log label (expected, actual) in
          fail
    [] -> 
      case actual of
        a :: az ->
          let _ = Debug.log label (expected, actual) in
          fail
        [] ->
          next

toList : Bytes -> Maybe (List Int)
toList b =
  decode (loop (width b, []) (listStep unsignedInt8)) b

dump title x =
  let _ = Debug.log title (Hex.toString x) in x
