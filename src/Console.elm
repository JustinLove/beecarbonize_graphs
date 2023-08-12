port module Console exposing
  ( write
  , error
  , log
  , readFile
  , readFileBinary
  , writeFile
  , exit
  , Signal(..)
  , signal
  , Event(..)
  , event
  )

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode

write : String -> Cmd msg
write text =
  Encode.object
    [ ("kind", Encode.string "write")
    , ("text", Encode.string text)
    ]
    |> consoleCommand

error : String -> Cmd msg
error text =
  Encode.object
    [ ("kind", Encode.string "error")
    , ("text", Encode.string text)
    ]
    |> consoleCommand

log : String -> String -> Cmd msg
log filename text =
  Encode.object
    [ ("kind", Encode.string "log")
    , ("filename", Encode.string filename)
    , ("text", Encode.string text)
    ]
    |> consoleCommand

readFile : String -> Cmd msg
readFile filename =
  Encode.object
    [ ("kind", Encode.string "readFile")
    , ("filename", Encode.string filename)
    ]
    |> consoleCommand

readFileBinary : String -> Cmd msg
readFileBinary filename =
  Encode.object
    [ ("kind", Encode.string "readFileBinary")
    , ("filename", Encode.string filename)
    ]
    |> consoleCommand

writeFile : String -> String -> Cmd msg
writeFile filename text =
  Encode.object
    [ ("kind", Encode.string "writeFile")
    , ("filename", Encode.string filename)
    , ("text", Encode.string text)
    ]
    |> consoleCommand

exit : Cmd msg
exit =
  Encode.object
    [ ("kind", Encode.string "exit")
    ]
    |> consoleCommand

type Signal
  = SigInt

signal : Signal -> msg -> Sub msg
signal sig msg =
  case sig of
    SigInt -> consoleSigInt (always msg)

type Event
  = ReadFile String (Result String String)
  | ReadFileBinary String (Result String String)
  | WriteFile String (Result String Bool)

event : (Result Decode.Error Event -> msg) -> Sub msg
event tagger =
  consoleEvent (decodeEvent >> tagger)

decodeEvent : Decode.Value -> Result Decode.Error Event
decodeEvent thing =
  Decode.decodeValue eventDecoder thing

eventDecoder : Decode.Decoder Event
eventDecoder =
  (Decode.field "kind" Decode.string)
    |> Decode.andThen (\kind ->
      case kind of
        "readfile" ->
          Decode.map2 ReadFile
            (Decode.field "filename" Decode.string)
            (Decode.field "result" (decodeResult Decode.string))
        "readfileBinary" ->
          Decode.map2 ReadFileBinary
            (Decode.field "filename" Decode.string)
            (Decode.field "result" (decodeResult Decode.string))
        "writefile" ->
          Decode.map2 WriteFile
            (Decode.field "filename" Decode.string)
            (Decode.field "result" (decodeResult Decode.bool))
        _ ->
          Decode.fail kind
      )

decodeResult : Decode.Decoder a -> Decode.Decoder (Result String a)
decodeResult decoder=
  Decode.oneOf
    [ Decode.field "ok" decoder |> Decode.map Ok
    , Decode.field "err" Decode.string |> Decode.map Err
    ]

port consoleCommand : Value -> Cmd msg
port consoleSigInt : (Value -> msg) -> Sub msg
port consoleEvent : (Value -> msg) -> Sub msg
