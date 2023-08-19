module ResourceGraph exposing(resourceGraph)

import Beecarbonize exposing (..)

import DotLang exposing (..)
import Dict exposing (Dict)

type alias Translations = Dict String String

type alias GameData =
  { cards : List Card
  , events : List Event
  , translations : Translations
  }

resourceGraph : GameData -> String
resourceGraph game =
  Dot Digraph (Just (ID "ResourceGraph"))
    [ LooseAttr (attr "bgcolor" "gray30")
    , AttrStmt AttrGraph [ attr "rankdir" "LR" ]
    , AttrStmt AttrEdge [ attr "color" "white" ]
    , sectorCards game Industry "Industry" "fuchsia" "plum" |> SubgraphStmt
    , sectorCards game Environment "Environment" "green" "palegreen" |> SubgraphStmt
    , sectorCards game People "People" "gold" "goldenrod1" |> SubgraphStmt
    , sectorCards game Science "Science" "dodgerblue" "lightskyblue" |> SubgraphStmt
    , buildEdges game |> SubgraphStmt
    ]
    |> toString

sectorCards : GameData -> Sector -> String -> String -> String -> Subgraph
sectorCards game sector label color fillcolor =
  List.append
    [ LooseAttr (attr "color" color)
    , AttrStmt AttrNode
      [ attr "color" color
      , attr "shape" "box"
      , attr "style" "filled"
      , attr "fillcolor" fillcolor
      ]
    ]
    (cardList game.translations (game.cards |> cardsInSector sector |> exceptCard 310))
    |> Subgraph (Just (ID label))

buildEdges : GameData -> Subgraph
buildEdges game =
  List.append [ ]
    (List.concatMap (cardBuildEdges game) game.cards)
    |> Subgraph Nothing

cardBuildEdges : GameData -> Card -> List Stmt
cardBuildEdges game card =
  List.concatMap
    (cardBuildEdge game card)
    card.buildableCardsIds

cardBuildEdge : GameData -> Card -> Int -> List Stmt
cardBuildEdge game card id =
  case findCard game.cards id of
    Just target ->
      case (target.buildCost, target.rebate) of
        (Tokens bi bp bs, Tokens ri rp rs) ->
          [ resourceEdge industryEdgeColor card.id id bi ri
          , resourceEdge peopleEdgeColor card.id id bp rp
          , resourceEdge scienceEdgeColor card.id id bs rs
          ] |> List.filterMap identity
    Nothing ->
      []

industryEdgeColor =
  [ attr "color" "plum"
  , attr "fontcolor" "plum"
  ]
peopleEdgeColor =
  [ attr "color" "goldenrod1"
  , attr "fontcolor" "goldenrod1"
  ]
scienceEdgeColor =
  [ attr "color" "lightskyblue"
  , attr "fontcolor" "lightskyblue"
  ]

resourceEdge : List Attr -> Int -> Int -> Int -> Int -> Maybe Stmt
resourceEdge color from to build rebate =
  if build > 0 || rebate > 0 then
    List.append
      [ if build == 0 then
          attr "label" ""
        else
          attr "label" (String.fromInt build)
      , attr "arrowhead" ("onormal" ++ (String.repeat rebate "dot"))
      ]
      color
      |> EdgeStmtNode (nodeid from) (EdgeNode (nodeid to)) []
      |> Just
  else
    Nothing

findCard : List Card -> Int -> Maybe Card
findCard cards id =
  case cards of
    card :: rest ->
      if card.id == id then
        Just card
      else
        findCard rest id
    [] ->
      Nothing

cardsInSector : Sector -> List Card -> List Card
cardsInSector sec cards =
  List.filter (inSector sec) cards

inSector : Sector -> Card -> Bool
inSector sec {sector} =
  sec == sector

exceptCard : Int -> List Card -> List Card
exceptCard id cards =
  List.filter (\c -> c.id /= id) cards

cardList : Translations -> List Card -> List Stmt
cardList translations cards =
  List.map (cardNode translations) cards

objList : Translations -> List (GameObject o) -> List Stmt
objList translations objs =
  List.map (simpleNode translations) objs

trans : Translations -> String -> String
trans translations key =
  Dict.get key translations
    |> Maybe.withDefault key

cardNode : Translations -> Card -> Stmt
cardNode translations card =
  if card.cardType == Victory then
    victoryNode translations card
  else 
    simpleNode translations card

victoryNode : Translations -> GameObject o -> Stmt
victoryNode translations obj =
  NodeStmt (nodeid obj.id)
    [ attr "label" (trans translations obj.displayName)
    , attr "fillcolor" "khaki1"
    , attr "shape" "octagon"
    ]

simpleNode : Translations -> GameObject o -> Stmt
simpleNode translations obj =
  NodeStmt (nodeid obj.id) [attr "label" (trans translations obj.displayName)]

nodeid : Int -> NodeId
nodeid id =
  NodeId (NumeralID (toFloat id)) Nothing

attr : String -> String -> Attr
attr key value =
  Attr (ID key) (ID value)
