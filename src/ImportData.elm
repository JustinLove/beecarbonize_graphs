module ImportData exposing (..)

import Beecarbonize exposing (..)
import Beecarbonize.Decode as Decode
import Console

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Hex.Convert as Hex
import Json.Decode

main : Program () Model Msg
main = Platform.worker
  { init = init
  , update = update
  , subscriptions = subscriptions
  }

type alias Model =
  { fileCount : Int
  , filesRead : Int
  , cards : List Card
  }

type Msg
  = Exit
  | ConsoleEvent (Result Json.Decode.Error Console.Event)

type alias Id = Int

type File
  = Other String
  | Dir (List String)
  | CardFile Card

init : () -> (Model, Cmd Msg)
init _ =
  ( { fileCount = 0
    , filesRead = 0
    , cards = []
    }
  , Cmd.batch
    [ Console.write "start"
    , Console.readFile "files.txt"
    ]
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Exit ->
      (model, Console.exit)
    ConsoleEvent (Ok (Console.ReadFile name (Ok contents))) ->
      updateReadFile name contents model
    ConsoleEvent (Ok (Console.ReadFile name (Err err))) ->
      (model, Console.write ("Failed to read " ++ name ++ " : " ++ err))
    ConsoleEvent (Ok (Console.ReadFileBinary name (Ok contents))) ->
      updateReadFile name contents model
    ConsoleEvent (Ok (Console.ReadFileBinary name (Err err))) ->
      (model, Console.write ("Failed to read " ++ name ++ " : " ++ err))
    ConsoleEvent (Ok (Console.WriteFile name (Ok _))) ->
      (model, Cmd.none)
    ConsoleEvent (Ok (Console.WriteFile name (Err err))) ->
      (model, Console.write ("Failed to write " ++ name ++ " : " ++ err))
    ConsoleEvent (Err err) ->
      (model, Console.write ("event decode failed " ++ (Json.Decode.errorToString err)))

updateReadFile : String -> String -> Model -> (Model, Cmd Msg)
updateReadFile name contents model =
  case parseFile name contents of
    Ok (Other text) -> 
      let _ = Debug.log name text in
      {model | filesRead = model.filesRead + 1}
        |> checkDone
    Ok (CardFile card) -> 
      let _ = Debug.log name card.objectName in
      {model | filesRead = model.filesRead + 1, cards = card :: model.cards}
        |> checkDone
    Ok (Dir paths) -> 
      let
        proc = paths
          --|> List.drop 29
          --|> List.take 160
      in
      ( {model | fileCount = List.length proc}
      , proc
        |> Debug.log "paths"
        |> List.map Console.readFileBinary
        |> Cmd.batch
      )
    Err err ->
      ( model
      , Cmd.batch
        [ Console.exit
        , Console.write ("Failed to parse " ++ name ++ " : " ++ err)
        ]
      )

checkDone : Model -> (Model, Cmd Msg)
checkDone model =
  let _ = Debug.log "check" (model.fileCount, model.filesRead, List.length model.cards) in
  if model.filesRead >= model.fileCount then
    ( model
    , Cmd.batch
      (Console.exit :: (List.map (Console.write<<printCard) model.cards))
    )
  else
    (model, Cmd.none)

printCard : Card -> String
printCard card =
  String.join " "
    [ String.fromInt card.id
    , sectorString card.sector
    , cardTypeString card.cardType
    , String.fromInt card.dubiousValue
    , String.fromInt card.arbitraryValue
    , if card.randomBuild then "R" else " "
    , String.fromInt (List.length card.buildableCardsIds)
    , if card.replacedByBuild then "!" else " "
    --, "X"
    --, String.fromInt (List.length card.eventsOnDestruction)
    --, "e"
    --, String.fromInt (List.length card.eventChances)
    , card.displayName
    ]

cardTypeString : CardType -> String
cardTypeString ct = 
  case ct of
    Dubious -> "0d"
    Normal -> "1n"
    SomethingElse -> "2s"
    Starred -> "4*"
    Victory -> "5v"

sectorString : Sector -> String
sectorString sector = 
  case sector of
    Industry -> "I"
    Environment -> "E"
    People -> "P"
    Science -> "S"

parseFile : String -> String -> Result String File
parseFile filename contents =
  if filename == "files.txt" then
    contents
      |> String.trimRight
      |> String.split "\n"
      |> Dir
      |> Ok
  else if String.contains "card" filename then
    contents
      --|> Debug.log "contents"
      |> parseCard
      |> Result.map CardFile
  else
    Ok (Other contents)

parseCard : String -> Result String Card
parseCard contents =
  Hex.toBytes contents
    --|> Maybe.map (dump "contents")
    |> Maybe.andThen (Bytes.Decode.decode Decode.card)
    |> Result.fromMaybe ("decode failed" ++ contents)

dump title x =
  let _ = Debug.log title (Hex.toString x |> Hex.blocks 8) in x

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Console.signal Console.SigInt Exit
    , Console.event ConsoleEvent
    ]
