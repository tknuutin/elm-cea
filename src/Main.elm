module Main exposing (..)

import List exposing (map, range, any)
import Html exposing (Html, text, div, h1, p, button, Attribute)
import Html.Events exposing (onClick)
import Html.Attributes exposing (classList)
import Models exposing (NodeType(Wall, StartNode, TargetNode, EmptyNode), GridNode, GridRow, Model, NodePos)
import Pathfinder


type alias InitialPositions =
    { start : NodePos
    , target : NodePos
    , walls : List NodePos
    }


initialPositions : InitialPositions
initialPositions =
    { start = { x = 1, y = 8 }
    , target = { x = 8, y = 1 }
    , walls = [ { x = 2, y = 4 }, { x = 3, y = 4 }, { x = 4, y = 4 }, { x = 5, y = 4 }, { x = 6, y = 4 }, { x = 7, y = 4 } ]
    }


getNodeType : Int -> Int -> NodeType
getNodeType row column =
    let
        { start, target, walls } =
            initialPositions
    in
        -- this is idiotic
        if (column == start.x && row == start.y) then
            StartNode
        else if (column == target.x && row == target.y) then
            TargetNode
        else if (any (\wallPos -> wallPos.x == column && wallPos.y == row) walls) then
            Wall
        else
            EmptyNode


initGridNode : Int -> Int -> GridNode
initGridNode rowNum columnNum =
    { x = columnNum, y = rowNum, ntype = getNodeType rowNum columnNum }


initGridRow : Int -> GridRow
initGridRow rowNum =
    map (\column -> initGridNode rowNum column) (range 0 9)


init : ( Model, Cmd Msg )
init =
    let
        initGrid : List GridRow
        initGrid =
            map initGridRow (range 0 9)
    in
        ( { grid = initGrid, result = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = FindPath

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        debugmsg = Debug.log "debugging message" msg
    in
        case debugmsg of
            FindPath ->
                ( {
                    grid = model.grid,
                    result = Pathfinder.findPath model.grid
                }, Cmd.none )



---- VIEW ----


getClassFromType : NodeType -> String
getClassFromType ntype =
    case ntype of
        Wall ->
            "wall"

        StartNode ->
            "start"

        TargetNode ->
            "target"

        EmptyNode ->
            "empty"


gridRow : GridRow -> Html Msg
gridRow row =
    let
        gridNode : GridNode -> Html Msg
        gridNode node =
            div [ classList [ ( "node", True ), ( getClassFromType node.ntype, True ) ] ]
                []
    in
        div []
            (map (\nodeDef -> gridNode nodeDef) row)


gridView : Model -> List (Html Msg)
gridView model =
    map gridRow model.grid

getResultText : Maybe (List GridRow) -> String
getResultText grid =
    case grid of
        Just a ->
            toString a
        Nothing ->
            ""

view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "AStar test" ]
        , div []
            (gridView model)
        , p [] [ text "White is the start, green is the target."]
        , p [] [ text "Grey is wall."]
        , button [ onClick FindPath ]
            [ text "Find path!" ]
        , div []
            [ text (getResultText model.result) ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
