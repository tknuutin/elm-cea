module Pathfinder exposing (findPath)

import List.Extra exposing (find)
import Models exposing (NodeType(Wall, StartNode, TargetNode, EmptyNode), GridNode, GridRow, Model, NodePos, NodeWithPos)
import Array
import Debug


--import Set

import Tuple
import Dict exposing (Dict)
import EverySet


--import AllDict
-- import Math
-- asdasd


inf : Float
inf =
    1 / 0


geometricDistance : NodeWithPos a -> NodeWithPos a -> Float
geometricDistance a b =
    sqrt ((toFloat b.x - toFloat a.x) ^ 2 + (toFloat b.y - toFloat a.y) ^ 2)


toKey : GridNode -> String
toKey node =
    (toString node.x) ++ "," ++ (toString node.y)


findNode : (GridNode -> Bool) -> List GridRow -> Maybe GridNode
findNode finder grid =
    find finder (List.concatMap identity grid)


findByNodeType : NodeType -> (List GridRow -> Maybe GridNode)
findByNodeType a =
    let
        findSNode : GridNode -> Bool
        findSNode node =
            node.ntype == a
    in
        findNode findSNode


type alias PathEndPoints =
    { start : GridNode, target : GridNode }


findPathEndpoints : List GridRow -> Maybe PathEndPoints
findPathEndpoints grid =
    let
        start =
            findByNodeType StartNode grid

        target =
            findByNodeType TargetNode grid
    in
        -- this looks like shit
        Maybe.andThen (\s -> (Maybe.map (\t -> { start = s, target = t }) target)) start


type alias NodeDistMap =
    Dict String Float


type alias ARow =
    Array.Array GridNode


type alias AGrid =
    Array.Array ARow


type alias SearchArgs =
    { guard : Int
    , start : GridNode
    , target : GridNode
    , grid : AGrid
    , openSet : EverySet.EverySet GridNode
    , closedSet : EverySet.EverySet GridNode
    , cameFrom : Dict String GridNode
    , gScore : NodeDistMap
    , fScore : NodeDistMap
    , current : GridNode
    }


getOrDef : Dict.Dict comparable b -> comparable -> b -> b
getOrDef bmap key def =
    Maybe.withDefault def (Dict.get key bmap)


getFScore : Dict.Dict String Float -> String -> Float
getFScore score key =
    getOrDef score key inf


getGScore : Dict.Dict String Float -> String -> Float
getGScore =
    getFScore



-- same defaults


lowestFScore : List GridNode -> NodeDistMap -> GridNode -> GridNode
lowestFScore openSet fScore current =
    let
        minFScore : GridNode -> Float
        minFScore node =
            getFScore fScore (toKey node)
    in
        Maybe.withDefault current (List.Extra.minimumBy minFScore openSet)


appendMaybe : Maybe a -> List a -> List a
appendMaybe newNode nodes =
    case newNode of
        Just val ->
            val :: nodes

        Nothing ->
            nodes


appendMaybeMany : List (Maybe a) -> List a -> List a
appendMaybeMany newNodes nodes =
    List.foldl appendMaybe nodes newNodes


getXY : AGrid -> Int -> Int -> Maybe GridNode
getXY grid x y =
    Maybe.andThen (\row -> (Array.get x row)) (Array.get y grid)


getNeighbors : AGrid -> Int -> Int -> List GridNode
getNeighbors grid x y =
    let
        west =
            getXY grid (x - 1) y

        east =
            getXY grid (x + 1) y

        north =
            getXY grid x (y - 1)

        south =
            getXY grid x (y + 1)
    in
        appendMaybeMany [ west, east, north, south ] []


getTotalDistanceAndNode : NodeDistMap -> GridNode -> GridNode -> ( GridNode, Float )
getTotalDistanceAndNode gScore current node =
    let
        distanceEstimate : Float
        distanceEstimate =
            (getGScore gScore (toKey current)) + (geometricDistance current node)
    in
        ( node, distanceEstimate )


buildArgs : GridNode -> GridNode -> Int -> AGrid -> EverySet.EverySet GridNode -> EverySet.EverySet GridNode -> Dict String GridNode -> NodeDistMap -> NodeDistMap -> GridNode -> SearchArgs
buildArgs start target guard grid openSet closedSet cameFrom gScore fScore current =
    { start = start
    , target = target
    , guard = guard
    , grid = grid
    , openSet = openSet
    , closedSet = closedSet
    , cameFrom = cameFrom
    , gScore = gScore
    , fScore = fScore
    , current = start
    }

searchIterateFinalize : (GridNode, Float) -> SearchArgs -> SearchArgs
searchIterateFinalize smallestNeighborDist args =
    let
        { guard, start, target, grid, cameFrom, openSet, closedSet, gScore, fScore, current } = args

        nextNeighbor =
            Tuple.first (Debug.log "smallest n" smallestNeighborDist)

        distanceToNext =
            Tuple.second smallestNeighborDist

        newGScore : NodeDistMap
        newGScore =
            Dict.insert (toKey nextNeighbor) distanceToNext gScore

        newFScore : NodeDistMap
        newFScore =
            Dict.insert (toKey nextNeighbor) (distanceToNext + (geometricDistance nextNeighbor target)) fScore
        foo = Debug.log "fscores after set" newFScore

        newCameFrom : Dict String GridNode
        newCameFrom =
            Dict.insert (toKey nextNeighbor) current cameFrom
    in
        { start = start
        , guard = guard + 1
        , closedSet = closedSet
        , openSet = openSet
        , target = target
        , grid = grid
        , cameFrom = newCameFrom
        , gScore = newGScore
        , fScore = newFScore
        , current = current
        }

searchIterate : SearchArgs -> Maybe SearchArgs
searchIterate args =
    let
        { guard, start, target, grid, cameFrom, gScore, fScore } =
            args

        current = Debug.log "current!!" args.current

        openSet =
            EverySet.remove current args.openSet

        closedSet =
            EverySet.insert current args.closedSet

        neighbors =
            getNeighbors grid current.x current.y

        notInClosedSet : GridNode -> Bool
        notInClosedSet node =
            not (EverySet.member node closedSet)

        unknownNeighbors =
            List.filter notInClosedSet neighbors

        openNodes =
            EverySet.union openSet (EverySet.fromList unknownNeighbors)

        unknownNeighborsWithRealDistance = Debug.log "u nebrs"
            (List.map (\node -> getTotalDistanceAndNode gScore current node) unknownNeighbors)

        smallestDistanceNeighbor =
            List.Extra.minimumBy Tuple.second unknownNeighborsWithRealDistance
    in
        case smallestDistanceNeighbor of
            Just a ->
                Just (searchIterateFinalize a (buildArgs start target guard grid openNodes closedSet cameFrom gScore fScore current))
            Nothing ->
                Nothing

dlog : String -> a -> a
dlog = Debug.log

search : SearchArgs -> Maybe (List GridRow)
search args =
    let
        { start, target, grid, openSet, closedSet, cameFrom, gScore, fScore, current, guard } =
            args

        ol = Debug.log "openset length" (EverySet.size openSet)

        newCurrent =
            Debug.log "in search, new current" (lowestFScore (EverySet.toList openSet) fScore current)
    in
        if (newCurrent.x == target.x && newCurrent.y == target.y) then
            Just [ [ { x = 0, y = 0, ntype = EmptyNode } ] ]
            -- victory!
        else if (guard >= 5) then
            dlog ("search: guard!!! " ++ (toString guard)) Nothing
        else
            let
                newArgs =
                    searchIterate (buildArgs start target (guard + 1) grid openSet closedSet cameFrom gScore fScore newCurrent)
            in
                case newArgs of
                    Just a ->
                        let
                            foo = dlog ("search: newargs, call again " ++ (toString (EverySet.size openSet))) 3
                        in
                            search a

                    Nothing ->
                        dlog ("search: newargs are nothing " ++ (toString guard)) Nothing 


findPathWithPoints : PathEndPoints -> List GridRow -> Maybe (List GridRow)
findPathWithPoints points rowList =
    let
        { start, target } =
            points

        grid =
            Array.fromList (List.map Array.fromList rowList)

        openSet : EverySet.EverySet GridNode
        openSet =
            EverySet.fromList [ start ]

        closedSet : EverySet.EverySet GridNode
        closedSet =
            EverySet.empty

        cameFrom : Dict String GridNode
        cameFrom =
            Dict.empty

        gScore =
            Dict.fromList [ ( toKey start, 0 ) ]

        fScore =
            Dict.fromList [ ( toKey start, geometricDistance start target ) ]
    in
        search (buildArgs start target 0 grid openSet closedSet cameFrom gScore fScore start)


findPath : List GridRow -> Maybe (List GridRow)
findPath grid =
    let
        endPoints : Maybe PathEndPoints
        endPoints =
            findPathEndpoints grid
    in
        case endPoints of
            Just points ->
                findPathWithPoints points grid

            Nothing ->
                Nothing
