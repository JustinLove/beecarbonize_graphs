module DecodeEventTest exposing (..)

import Beecarbonize exposing (..)
import Beecarbonize.Decode exposing (..)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (decode)
import Bytes.Encode as Encode

import Hex.Convert as Hex

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "tests"
    [ describe "event"
      --[ test "hurricanes" <| \_ ->
        --decode event hurricanes
          --|> Expect.equal (Just hurricanesEvent)
      [ test "pandemic" <| \_ ->
        decode event pandemic
          |> Expect.equal (Just pandemicEvent)
      ]
    ]


hurricanes = Hex.toBytes hurricanesString |> Maybe.withDefault (Encode.encode (Encode.string ""))
hurricanesString = "000000000000000000000000010000000100000088000000000000000a0000006576656e745f313030310000e90300001d0000006576656e74732f64657374727563746976655f687572726963616e657300000001000000010000004600000001000000000000000000000000000000000000000000000000000000020000000200000002000000000000000000000000000000010000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000003000000fa000000000000003f420f000000000000000000000000000000000000000000f000000000000000"
hurricanesEvent =
  { objectName = "event_1001"
  , id = 1001
  , displayName = "events/destructive_hurricanes"
  , duration = 70
  , eventType = Negative
  , v2 = 1
  , v3 = 1
  , everyRoundEffect = noEffect
  , expireEffect = loseResources (Tokens 2 2 0)
  , solveEffect = gainResources (Tokens 0 2 0)
  , insolvencyEffect = noEffect
  , solutionCost = Tokens 0 1 3
  , tippingPoint = TippingPoint 250
  , minRounds = 0
  , probability = 0
  , emissionRange = (0, 999999)
  , v5 = 0
  , spriteId = 240
  }

pandemic = Hex.toBytes pandemicString |> Maybe.withDefault (Encode.encode (Encode.string ""))
pandemicString = "000000000000000000000000010000000100000088000000000000000a0000006576656e745f313031300000f20300000f0000006576656e74732f70616e64656d69630001000000010000002d0000000100000000000000000000000000000000000000000000000000000002000000000000000a0000000000000000000000000000000000000000000000000000000000000000000000000000000300000000000000000000000000000003000000ed000000ee000000f10000000000000000000000010000000500000000000000000000003f420f00000000000ad7a33c5000000000000000000000000000000000000000"
pandemicEvent =
  { objectName = "event_1010"
  , id = 1010
  , displayName = "events/pandemic"
  , duration = 45
  , eventType = Negative
  , v2 = 1
  , v3 = 1
  , everyRoundEffect = noEffect
  , expireEffect = loseResources (Tokens 0 10 0)
  , solveEffect = noEffect
  , insolvencyEffect = destroyCards [237,238,241]
  , solutionCost = Tokens 0 1 5
  , tippingPoint = Random
  , probability = (imprecise 0.02)
  , minRounds = 80
  , emissionRange = (0, 999999)
  , v5 = 0
  , spriteId = 0
  }

imprecise : Float -> Float
imprecise x =
  x
    |> Encode.float32 LE
    |> Encode.encode
    |> Decode.decode (Decode.float32 LE)
    |> Maybe.withDefault x

dump title x =
  let _ = Debug.log title (Hex.toString x) in x
