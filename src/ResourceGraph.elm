module ResourceGraph exposing(resourceGraph, eventGraph)

import Beecarbonize exposing (..)

import DotLang exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)

type alias Translations = Dict String String

type alias GameData =
  { cards : List Card
  , events : List Event
  , translations : Translations
  }

resourceGraph : GameData -> String
resourceGraph g =
  let game = {g | cards = g.cards |> exceptCard 310} in
  Dot Digraph (Just (ID "ResourceGraph"))
    [ LooseAttr (attr "bgcolor" "gray30")
    , AttrStmt AttrGraph [ attr "rankdir" "LR" ]
    , AttrStmt AttrEdge [ attr "color" "white" ]
    , sectorCards game Industry "Industry" "fuchsia" "plum" |> SubgraphStmt
    , sectorCards game Environment "Environment" "green" "palegreen" |> SubgraphStmt
    , sectorCards game People "People" "gold" "goldenrod1" |> SubgraphStmt
    , sectorCards game Science "Science" "dodgerblue" "lightskyblue" |> SubgraphStmt
    , cardsBuildEdges game |> SubgraphStmt
    ]
    |> toString

eventGraph : GameData -> String
eventGraph g =
  let
    ids = relevantIds g
    game =
      { g
      | cards = g.cards |> onlyIds ids
      , events = g.events |> onlyIds ids
      }
  in
  Dot Digraph (Just (ID "EventResourceGraph"))
    [ LooseAttr (attr "bgcolor" "gray30")
    , AttrStmt AttrGraph [ attr "rankdir" "LR" ]
    , AttrStmt AttrEdge [ attr "color" "white" ]
    , sectorCards game Industry "Industry" "fuchsia" "plum" |> SubgraphStmt
    , sectorCards game Environment "Environment" "green" "palegreen" |> SubgraphStmt
    , sectorCards game People "People" "gold" "goldenrod1" |> SubgraphStmt
    , sectorCards game Science "Science" "dodgerblue" "lightskyblue" |> SubgraphStmt
    , eventNodes game |> SubgraphStmt
    , eventsSolveEdges game |> SubgraphStmt
    , triggerEdges game |> SubgraphStmt
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
    (cardList game.translations (game.cards |> cardsInSector sector))
    |> Subgraph (Just (ID label))

eventNodes : GameData -> Subgraph
eventNodes game =
  List.append
    [ LooseAttr (attr "color" "")
    , AttrStmt AttrNode
      [ attr "color" "gray10"
      , attr "shape" "ellipse"
      , attr "style" "filled"
      , attr "fontcolor" "white"
      ]
    , AttrStmt AttrEdge
      [ attr "color" "gold3"
      , attr "dir" "both"
      , attr "arrowtail" "odiamond"
      ]
    ]
    (eventList game.translations (game.events))
    |> Subgraph (Just (ID "Events"))

cardsBuildEdges : GameData -> Subgraph
cardsBuildEdges game =
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

eventsSolveEdges : GameData -> Subgraph
eventsSolveEdges game =
  List.append [ ]
    (List.concatMap (eventSolveEdges game) game.events)
    |> Subgraph Nothing

eventSolveEdges : GameData -> Event -> List Stmt
eventSolveEdges game event =
  List.concatMap identity
    [ effectSolveEdges game event event.solveEffect
    , effectEdges game event event.everyRoundEffect
    , effectEdges game event event.insolvencyEffect
    , effectEdges game event event.expireEffect
    ]

effectSolveEdges : GameData -> Event -> Effect -> List Stmt
effectSolveEdges game event effect =
  case effect.effectType of
    NoEffect -> []
    GainResources -> []
    LoseResources -> []
    DestroyCards -> []
    CreateCards ->
      List.concatMap (eventSolveEdge game event) effect.targetIds
    ModifyEmissions -> []
    LoseGame -> []

eventSolveEdge : GameData -> Event -> Int -> List Stmt
eventSolveEdge game event id =
  case findCard game.cards id of
    Just target ->
      case (event.solutionCost, target.rebate) of
        (Tokens bi bp bs, Tokens ri rp rs) ->
          [ resourceEdge industryEdgeColor event.id id bi ri
          , resourceEdge peopleEdgeColor event.id id bp rp
          , resourceEdge scienceEdgeColor event.id id bs rs
          ] |> List.filterMap identity
    Nothing ->
      []

effectEdges : GameData -> Event -> Effect -> List Stmt
effectEdges game event effect =
  case effect.effectType of
    NoEffect -> []
    GainResources -> []
    LoseResources -> []
    DestroyCards -> []
    CreateCards ->
      List.concatMap (eventEdge game event) effect.targetIds
    ModifyEmissions -> []
    LoseGame ->
      [ loseNode event
      , EdgeStmtNode (nodeid event.id) (EdgeNode (loseid event.id)) [] []
      ]

eventEdge : GameData -> Event -> Int -> List Stmt
eventEdge game event id =
  [ EdgeStmtNode (nodeid event.id) (EdgeNode (nodeid id)) [] []
  ]

triggerEdges : GameData -> Subgraph
triggerEdges game =
  List.append
    [ AttrStmt AttrNode
      [ attr "shape" "none"
      , attr "fontcolor" "white"
      ]
    ]
    (List.concatMap (triggerEdge game) game.events)
    |> Subgraph Nothing

triggerEdge : GameData -> Event -> List Stmt
triggerEdge game event =
  case event.tippingPoint of
    TippingPoint level ->
      [ EdgeStmtNode (tipid level) (EdgeNode (nodeid event.id)) [] []
      ]
    Random -> []

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
findCard = findGameObject

findEvent : List Event -> Int -> Maybe Event
findEvent = findGameObject

findGameObject : List (GameObject o) -> Int -> Maybe (GameObject o)
findGameObject objs id =
  case objs of
    obj :: rest ->
      if obj.id == id then
        Just obj
      else
        findGameObject rest id
    [] ->
      Nothing

cardsInSector : Sector -> List Card -> List Card
cardsInSector sec cards =
  List.filter (inSector sec) cards

inSector : Sector -> Card -> Bool
inSector sec {sector} =
  sec == sector

onlyIds : Set Int -> List (GameObject o) -> List (GameObject o)
onlyIds ids objs =
  objs
    |> List.filter (\c -> Set.member c.id ids)

relevantIds : GameData -> Set Int
relevantIds game =
  Set.union
    (tippingPointIds game.events)
    (relevantCreateIds game)

relevantCreateIds : GameData -> Set Int
relevantCreateIds game =
  Set.union
    (createIds game.events)
    (creatingIds game.events)

createIds : List Event -> Set Int
createIds events =
  events
    |> List.concatMap (\e -> [e.solveEffect, e.everyRoundEffect, e.expireEffect, e.insolvencyEffect])
    |> List.filter (\e -> e.effectType == CreateCards)
    |> List.concatMap .targetIds
    |> Set.fromList

creatingIds : List Event -> Set Int
creatingIds events =
  events
    |> List.filter (\e ->
      e.solveEffect.effectType == CreateCards ||
      e.everyRoundEffect.effectType == CreateCards ||
      e.expireEffect.effectType == CreateCards ||
      e.insolvencyEffect.effectType == CreateCards
    )
    |> List.map .id
    |> Set.fromList

tippingPointIds : List Event -> Set Int
tippingPointIds events =
  events
    |> List.filter hasTippingPoint
    |> List.map .id
    |> Set.fromList

hasTippingPoint : Event -> Bool
hasTippingPoint event =
  case event.tippingPoint of
    TippingPoint _ -> True
    Random -> False

exceptCard : Int -> List Card -> List Card
exceptCard id cards =
  List.filter (\c -> c.id /= id) cards

cardList : Translations -> List Card -> List Stmt
cardList translations cards =
  List.map (cardNode translations) cards

eventList : Translations -> List Event -> List Stmt
eventList translations events =
  List.map (eventNode translations) events

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
    regularCardNode translations card

eventNode : Translations -> Event -> Stmt
eventNode = simpleNode

victoryNode : Translations -> GameObject o -> Stmt
victoryNode translations obj =
  NodeStmt (nodeid obj.id)
    [ attr "label" (trans translations obj.displayName)
    , attr "fillcolor" "khaki1"
    , attr "shape" "octagon"
    ]

loseNode : GameObject o -> Stmt
loseNode obj =
  NodeStmt (loseid obj.id)
    [ attr "label" "Lose"
    , attr "style" "filled"
    , attr "fillcolor" "crimson"
    , attr "shape" "octagon"
    ]

simpleNode : Translations -> GameObject o -> Stmt
simpleNode translations obj =
  NodeStmt (nodeid obj.id) [attr "label" (trans translations obj.displayName)]

regularCardNode : Translations -> Card -> Stmt
regularCardNode translations card =
  NodeStmt (nodeid card.id) [attr "label" ((trans translations card.displayName) ++ emissionsNote card) ]

emissionsNote : Card -> String
emissionsNote card =
  if card.instantEmissions == 0 then
    ""
  else
    " (" ++ (String.fromInt card.instantEmissions) ++ ")"

nodeid : Int -> NodeId
nodeid id =
  NodeId (NumeralID (toFloat id)) Nothing

loseid : Int -> NodeId
loseid id =
  NodeId (ID ("Lose" ++ String.fromInt id)) Nothing

tipid : Int -> NodeId
tipid id =
  NodeId (ID ("Tipping Point " ++ String.fromInt id)) Nothing

attr : String -> String -> Attr
attr key value =
  Attr (ID key) (ID value)
