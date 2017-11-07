module Models exposing (..)


type NodeType
    = Wall
    | TargetNode
    | StartNode
    | EmptyNode


type alias NodeWithPos a =
    { a
        | x : Int
        , y : Int
    }


type alias NodePos =
    NodeWithPos {}


type alias GridNode =
    NodeWithPos { ntype : NodeType }


type alias GridRow =
    List GridNode


type alias Model =
    { grid : List GridRow
    , result : Maybe (List GridRow)
    }
