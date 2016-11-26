module Types exposing (GridPosition, GridItem)


type alias GridPosition =
    { x : Int
    , y : Int
    }


type alias GridItem a =
    { a
        | width : Int
        , height : Int
    }
