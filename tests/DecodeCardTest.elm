module DecodeCardTest exposing (..)

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
    [ describe "card"
      [ test "industry" <| \_ ->
        decode card industry
          |> Expect.equal (Just industryCard)
      , test "cooling" <| \_ ->
        decode card cooling
          |> Expect.equal (Just coolingCard)
      , test "coal" <| \_ ->
        decode card coal
          |> Expect.equal (Just coalCard)
      , test "thermo" <| \_ ->
        decode card thermo
          |> Expect.equal (Just thermoCard)
      , test "mirrors" <| \_ ->
        decode card mirrors
          |> Expect.equal (Just mirrorsCard)
      , test "gentic" <| \_ ->
        decode card genetic
          |> Expect.equal (Just geneticCard)
      , test "idea" <| \_ ->
        decode card idea
          |> Expect.equal (Just ideaCard)
      ]
{-
    , describe "bytes"
      [ test "floating point rep" <| \_ ->
        Encode.encode (Encode.float32 LE 1.0)
          |> dump "encode float"
          |> Expect.equal (Encode.encode (Encode.unsignedInt32 LE 0x3f800000))
      , test "floating point decode" <| \_ ->
        (Encode.encode (Encode.unsignedInt32 LE 0x3f800000))
          |> dump "encode int"
          |> (Decode.decode (Decode.float32 LE))
          |> Expect.equal (Just 1.0)
      ]
      -}
    ]



empty = Encode.encode (Encode.string "")
industry = Hex.toBytes industryString |> Maybe.withDefault (Encode.encode (Encode.string ""))
industryString = "00000000000000000000000001000000010000007D0200000000000008000000636172645F3130306400000014000000636172645F6E616D652F696E64757374727932300200000000000000000000000000000001000000000000000000000000000000030000006F000000680000006B0000001E0000000000803F1400000001000000010000000400000000000000000000000100000003000000890300000AD7A33CA30300000AD7A33C990300000AD7A33C02000000FE030000FF030000000000003401000000000000010000000A000000696E64757374727932300000"
industryCard =
  { objectName = "card_100"
  , id = 100
  , displayName = "card_name/industry20"
  , buildCost = Tokens 2 0 0
  , dubiousValue = 0
  , rebate = Tokens 1 0 0
  , instantEmissions = 0
  , buildableCardsIds = [111, 104, 107]
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

cooling = Hex.toBytes coolingString |> Maybe.withDefault (Encode.encode (Encode.string ""))
coolingString = "00000000000000000000000001000000010000007d0200000000000008000000636172645f343632ce0100001b000000636172645f6e616d652f7261646961746976655f636f6f6c696e6700000000000400000007000000000000000000000000000000000000000000000000000000c8000000cdcccc3efdffffff040000000000000000000000000000000100000001000000f80300000ad7a33c0000000000000000650100000000000001000000110000007261646961746976655f636f6f6c696e67000000"
coolingCard =
  { objectName = "card_462"
  , id = 462 
  , displayName = "card_name/radiative_cooling"
  , buildCost = Tokens 0 4 7
  , dubiousValue = 0
  , rebate = Tokens 0 0 0
  , instantEmissions = 0
  , buildableCardsIds = []
  , buildTime = 200
  , speed = (imprecise 0.4)
  , emissions = -3
  , sector = Science
  , replacedByBuild = False
  , randomBuild = False
  , cardType = Normal
  , eventChances =
    [ EventChance 1016 (imprecise 0.02)
    ]
  , eventsOnDestruction = []
  , spriteId = 357
  , arbitraryValue = 1
  , internalName = "radiative_cooling"
  }


coal = Hex.toBytes coalString |> Maybe.withDefault (Encode.encode (Encode.string ""))
coalString = "00000000000000000000000001000000010000007d0200000000000008000000636172645f3130316500000013000000636172645f6e616d652f6d6f72655f636f616c000100000001000000000000000000000002000000000000000000000000000000010000006600000019000000cdcccc3e0a000000010000000100000004000000000000000000000001000000010000008f0300000ad723bc0300000088030000fe030000ff030000000000006f0100000000000002000000090000006d6f72655f636f616c000000"
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

thermo = Hex.toBytes thermoString |> Maybe.withDefault (Encode.encode (Encode.string ""))
thermoString = "00000000000000000000000001000000010000007d0200000000000008000000636172645f343634d00100001e000000636172645f6e616d652f706c616e65746172795f746865726d6f73746174000014000000140000001e000000000000000000000000000000000000000000000000000000900100000000803fecffffff040000000000000000000000000000000500000000000000000000000000000073010000000000000100000014000000706c616e65746172795f746865726d6f73746174"
thermoCard =
  { objectName = "card_464"
  , id = 464
  , displayName = "card_name/planetary_thermostat"
  , buildCost = Tokens 20 20 30
  , dubiousValue = 0
  , rebate = Tokens 0 0 0
  , instantEmissions = 0
  , buildableCardsIds = []
  , buildTime = 400
  , speed = 1.0
  , emissions = -20
  , sector = Science
  , replacedByBuild = False
  , randomBuild = False
  , cardType = Victory
  , eventChances = []
  , eventsOnDestruction = []
  , spriteId = 371
  , arbitraryValue = 1
  , internalName = "planetary_thermostat"
  }


mirrors = Hex.toBytes mirrorsString |> Maybe.withDefault (Encode.encode (Encode.string ""))
mirrorsString = "00000000000000000000000001000000010000007d0200000000000008000000636172645f343633cf01000017000000636172645f6e616d652f73706163655f6d6972726f7273000f0000000a0000000000000000000000000000000000000000000000fa00000001000000d0010000220100009a99193ffbffffff040000000000000001000000000000000400000005000000fa0300008fc2f53cf70300000ad7a33cfb0300000ad7a33cf50300000ad7233cd70300000ad7233c0000000000000000bc00000000000000020000000d00000073706163655f6d6972726f7273000000"
mirrorsCard =
  { objectName = "card_463"
  , id = 463
  , displayName = "card_name/space_mirrors"
  , buildCost = Tokens 15 10 0
  , dubiousValue = 0
  , rebate = Tokens 0 0 0
  , instantEmissions = 250
  , buildableCardsIds = [464]
  , buildTime = 290
  , speed = (imprecise 0.6)
  , emissions = -5
  , sector = Science
  , replacedByBuild = True
  , randomBuild = False
  , cardType = Starred
  , eventChances =
    [ EventChance 1018 (imprecise 0.03)
    , EventChance 1015 (imprecise 0.02)
    , EventChance 1019 (imprecise 0.02)
    , EventChance 1013 (imprecise 0.01)
    , EventChance 983 (imprecise 0.01)
    ]
  , eventsOnDestruction = []
  , spriteId = 188
  , arbitraryValue = 2
  , internalName = "space_mirrors"
  }

genetic = Hex.toBytes geneticString |> Maybe.withDefault (Encode.encode (Encode.string ""))
geneticString = "00000000000000000000000001000000010000007d0200000000000008000000636172645f343539cb0100001a000000636172645f6e616d652f67656e65746963616c5f75746f7069610000000000000900000009000000000000000000000000000000000000000000000000000000aa0000009a99193f01000000020000000000000000000000000000000200000006000000aa0300000ad7a3bca20300000ad7a3bc960300000ad7a3bc920300000ad7a3bcb30300000ad723bcf20300000ad7a3bc0000000000000000d100000000000000010000001000000067656e65746963616c5f75746f706961"
geneticCard =
  { objectName = "card_459"
  , id = 459
  , displayName = "card_name/genetical_utopia"
  , buildCost = Tokens 0 9 9
  , dubiousValue = 0
  , rebate = Tokens 0 0 0
  , instantEmissions = 0
  , buildableCardsIds = []
  , buildTime = 170
  , speed = (imprecise 0.6)
  , emissions = 1
  , sector = People
  , replacedByBuild = False
  , randomBuild = False
  , cardType = SomethingElse
  , eventChances =
    [ EventChance 938 (imprecise -0.02)
    , EventChance 930 (imprecise -0.02)
    , EventChance 918 (imprecise -0.02)
    , EventChance 914 (imprecise -0.02)
    , EventChance 947 (imprecise -0.01)
    , EventChance 1010 (imprecise -0.02)
    ]
  , eventsOnDestruction = []
  , spriteId = 209
  , arbitraryValue = 1
  , internalName = "genetical_utopia"
  }


idea = Hex.toBytes ideaString |> Maybe.withDefault (Encode.encode (Encode.string ""))
ideaString = "00000000000000000000000001000000010000007d0200000000000008000000636172645f323031c900000018000000636172645f6e616d652f696465615f696e63756261746f72000000000200000000000000010000000000000000000000000000000000000005000000eb000000cb000000cd000000fa000000f3000000320000000000803e0100000002000000000000000000000001000000010000000000000000000000000000000000000000000000010000000e000000696465615f696e63756261746f720000"
ideaCard =
  { objectName = "card_201"
  , id = 201
  , displayName = "card_name/idea_incubator"
  , buildCost = Tokens 0 2 0
  , dubiousValue = 1
  , rebate = Tokens 0 0 0
  , instantEmissions = 0
  , buildableCardsIds = [235,203,205,250,243]
  , buildTime = 50
  , speed = 0.25
  , emissions = 1
  , sector = People
  , replacedByBuild = False
  , randomBuild = True
  , cardType = Normal
  , eventChances = [ ]
  , eventsOnDestruction = []
  , spriteId = 0
  , arbitraryValue = 1
  , internalName = "idea_incubator"
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
