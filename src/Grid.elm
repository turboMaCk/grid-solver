module Grid
    exposing
        ( GridPosition
        , isInRange
        , isTaken
        , getPosition
        , getPositions
        )

{-| Some desc

# Definition
@docs GridPosition

# Api
@docs isInRange, isTaken, getPosition, getPositions
-}

import List
import Maybe


{-|
-}
type alias GridPosition =
    { x : Int
    , y : Int
    }


{-|
-}
isInRange : Int -> Int -> Int -> Bool
isInRange min max value =
    value >= min && value < max


{-|
-}
isTaken :
    List ( GridPosition, { a | height : Int, width : Int } )
    -> GridPosition
    -> Bool
isTaken grid position =
    let
        conflictX pos item =
            isInRange pos.x (item.width + pos.x) position.x

        conflictY pos item =
            isInRange pos.y (item.height + pos.y) position.y
    in
        List.length (List.filter (\( position, item ) -> (conflictX position item) && (conflictY position item)) grid) > 0


{-|
-}
getPosition :
    Int
    -> List ( GridPosition, { a | width : Int, height : Int } )
    -> { a | width : Int, height : Int }
    -> ( GridPosition, { a | width : Int, height : Int } )
getPosition perRow grid item =
    let
        last =
            List.reverse grid |> List.head

        nextPosition position =
            if position.x + item.width < perRow then
                { x = position.x + 1, y = position.y }
            else
                { x = 0, y = position.y + 1 }

        firstTryPosition =
            case last of
                Just ( lastPosition, lastItem ) ->
                    nextPosition
                        { x = lastPosition.x + lastItem.width - 1
                        , y = lastPosition.y
                        }

                Nothing ->
                    { x = 0, y = 0 }

        tryPosition position =
            case isTaken grid position of
                True ->
                    tryPosition (nextPosition position)

                False ->
                    position
    in
        ( (tryPosition firstTryPosition), item )


{-|
-}
getPositions :
    Int
    -> List { a | width : Int, height : Int }
    -> List ( GridPosition, { a | width : Int, height : Int } )
getPositions perRow list =
    List.foldl (\item acc -> acc ++ [ (getPosition perRow acc item) ]) [] list
