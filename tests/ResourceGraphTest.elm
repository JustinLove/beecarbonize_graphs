module ResourceGraphTest exposing (..)

import Beecarbonize exposing (..)
import ResourceGraph exposing (..)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (decode)
import Bytes.Encode as Encode
import Dict exposing (Dict)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "tests"
    [ describe "resource graph"
      [ test "true" <| \_ -> Expect.equal True True
      --[ test "graph" <| \_ ->
       -- resourceGraph model
        --  |> Expect.equal ""
      ]
    ]

model =
  { cards = [industryCard, coalCard]
  , events = []
  , translations =
    [ ("card_name/industry20", "Industry 20th Century")
    , ("card_name/more_coal", "Coal Power Plants")
    ] |> Dict.fromList
  }

industryCard =
  { objectName = "card_100"
  , id = 100
  , displayName = "card_name/industry20"
  , buildCost = Tokens 2 0 0
  , dubiousValue = 0
  , rebate = Tokens 1 0 0
  , instantEmissions = 0
  , buildableCardsIds = [111, 104, 107,101]
  , buildTime = 30
  , speed = 1.0
  , emissions = 20
  , sector = Industry
  , replacedByBuild = False
  , randomBuild = False
  , cardType = Normal
  , eventChances =
    [ EventChance 905 (imprecise 0.02)
    , EventChance 931 (imprecise 0.02)
    , EventChance 921 (imprecise 0.02)
    ]
  , eventsOnDestruction = [1022, 1023]
  , spriteId = 308
  , arbitraryValue = 1
  , internalName = "industry20"
  }

coalCard =
  { objectName = "card_101"
  , id = 101 
  , displayName = "card_name/more_coal"
  , buildCost = Tokens 1 1 0
  , dubiousValue = 0
  , rebate = Tokens 2 0 0
  , instantEmissions = 0
  , buildableCardsIds = [102]
  , buildTime = 25
  , speed = (imprecise 0.4)
  , emissions = 10 
  , sector = Industry
  , replacedByBuild = False
  , randomBuild = False
  , cardType = Normal
  , eventChances =
    [ EventChance 911 (imprecise -0.01)
    ]
  , eventsOnDestruction = [904,1022, 1023]
  , spriteId = 367
  , arbitraryValue = 2
  , internalName = "more_coal"
  }

imprecise : Float -> Float
imprecise x =
  x
    |> Encode.float32 LE
    |> Encode.encode
    |> Decode.decode (Decode.float32 LE)
    |> Maybe.withDefault x
