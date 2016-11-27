module Grid
    exposing
        ( Position
        , Item
        , solve
        )

{-| Build awesome Grid layouts in Elm.

This library provides general abstraction for solving grid layout problems.
The idea is to use **ordered list of items with dimmensions** and **number of columns**
to solve whole layout of grid. It's designed to be easy to use and extremly deterministic
abstraction with simple integration to any Html, SVG or Canvas based APP.

# Definition
@docs Position, Item

# Api
@docs solve
-}

import List
import Maybe


-- Modules

import Utils exposing (..)
import Types exposing (GridPosition, GridItem)


-- Types


{-|
    type alias GridPosition =
        { x : Int
        , y : Int
        }
-}
type alias Position =
    Types.GridPosition


{-|
    type alias GridItem a =
        { a
            | width : Int
            , height : Int
        }
-}
type alias Item a =
    Types.GridItem a



-- Public Api


{-| This function is heart and the only public function
this library provides. It calculates grid layout
for given number of columns and items with dimmensions.
You can then use result (List of tuples with `Position` and `Item`)
in you're views to simply render layout as you wish.
Order of items is guaranteed to stay same.

- `Int` - number of Columns
- `List` - Grid items

## Four column example:

    -- Expected result:
    -- ================
    --    0 1 2 3
    -- 0 |A|A|A|A|
    -- 1 |A|A|A|A|
    -- 2 |B|B|C|C|
    -- 3 | | |C|C|

    solve 4
        [ { width = 4, height = 2 }
        , { width = 2, height = 1 }
        , { width = 2, height = 2 }
        ]
    ==
    [ ( { x = 0, y = 0 }, { width = 4, height = 2 } )
    , ( { x = 0, y = 2 }, { width = 2, height = 1 } )
    , ( { x = 2, y = 2 }, { width = 2, height = 2 } )
    ]


## Two column example:

    -- Expected result:
    -- ================
    --    0 1
    -- 0 |A|A|
    -- 1 |B| |
    -- 2 |B| |
    -- 3 |C|C|

    solve 2
        [ { width = 2, height = 1 }
        , { width = 1, height = 2 }
        , { width = 2, height = 1 }
        ]
    ==
    [ ( { x = 0, y = 0 }, { width = 2, height = 1 } )
    , ( { x = 0, y = 1 }, { width = 1, height = 2 } )
    , ( { x = 0, y = 3 }, { width = 2, height = 1 } )
    ]

## More complex layout:

    -- Expected result:
    -- ================
    --    0 1 2 3
    -- 0 |A|A|B|B|
    -- 1 |C|C|B|B|
    -- 2 | | |B|B|
    -- 3 |D|D|D|D|

    solve 4
        [ { width = 2, height = 1 }
        , { width = 2, height = 3 }
        , { width = 2, height = 1 }
        , { width = 4, height = 1 }
        ]
    ==
    [ ( { x = 0, y = 0 }, { width = 2, height = 1 } )
    , ( { x = 2, y = 0 }, { width = 2, height = 3 } )
    , ( { x = 0, y = 1 }, { width = 2, height = 1 } )
    , ( { x = 0, y = 3 }, { width = 4, height = 1 } )
    ]
-}
solve :
    Int
    -> List (Item a)
    -> List ( Position, Item a )
solve perRow list =
    List.foldl (\item acc -> acc ++ [ (getPosition perRow acc item) ]) [] list
