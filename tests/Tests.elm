module Tests exposing (..)

import Test exposing (..)
import Expect
import Tuple exposing (first)


-- Library

import Types exposing (..)
import Utils exposing (..)
import Grid exposing (solve)


-- Types


type alias Fixture =
    ( GridPosition, { width : Int, height : Int } )



-- Utils


createItem : Int -> Int -> Int -> Int -> Fixture
createItem x y width height =
    ( { x = x, y = y }, { width = width, height = height } )



-- Tests


all : Test
all =
    describe "all"
        [ rangeTest
        , isTakenTest
        , getPositionTest
        , solveTest
        ]


rangeTest : Test
rangeTest =
    describe "isInRange"
        [ test "2 is in range <1, 5]" <|
            \() ->
                (Expect.equal True
                    (isInRange 1 5 2)
                )
        , test "7 is not in range <1, 6]" <|
            \() ->
                (Expect.equal False
                    (isInRange 1 6 7)
                )
        , test "1.01 is in range <1, 2]" <|
            \() ->
                (Expect.equal True
                    (isInRange 1 2 1)
                )
        , test "1 is in range <1, 2]" <|
            \() ->
                (Expect.equal True
                    (isInRange 1 2 1)
                )
        , test "2 is not in range <1, 2]" <|
            \() ->
                (Expect.equal False
                    (isInRange 1 2 2)
                )
        ]


isTakenTest : Test
isTakenTest =
    let
        -- GRID STATE:
        -- ===========
        --    0 1 2 3
        -- 0 |A|A|A|A|
        -- 1 |B|B| | |
        -- 2 |B|B| | |
        state =
            [ (createItem 0 0 4 1)
            , (createItem 0 1 2 2)
            ]

        grid =
            isTaken state
    in
        describe "isTaken"
            [ test "0,0 is taken" <|
                \() ->
                    (Expect.equal True
                        (grid { x = 0, y = 0 })
                    )
            , test "1,0 is taken" <|
                \() ->
                    (Expect.equal True
                        (grid { x = 1, y = 0 })
                    )
            , test "2,0 is taken" <|
                \() ->
                    (Expect.equal True
                        (grid { x = 2, y = 0 })
                    )
            , test "3,0 is taken" <|
                \() ->
                    (Expect.equal True
                        (grid { x = 3, y = 0 })
                    )
            , test "4,0 is NOT taken (out of grid!)" <|
                \() ->
                    (Expect.equal False
                        (grid { x = 4, y = 0 })
                    )
            , test "0,1 is taken" <|
                \() ->
                    (Expect.equal True
                        (grid { x = 0, y = 1 })
                    )
            , test "1,1 is taken" <|
                \() ->
                    (Expect.equal True
                        (grid { x = 1, y = 1 })
                    )
            , test "2,1 is NOT taken" <|
                \() ->
                    (Expect.equal False
                        (grid { x = 2, y = 1 })
                    )
            , test "3,1 is NOT taken" <|
                \() ->
                    (Expect.equal False
                        (grid { x = 3, y = 1 })
                    )
            , test "0,2 is taken" <|
                \() ->
                    (Expect.equal True
                        (grid { x = 0, y = 2 })
                    )
            , test "1,2 is taken" <|
                \() ->
                    (Expect.equal True
                        (grid { x = 1, y = 2 })
                    )
            , test "2,2 is NOT taken" <|
                \() ->
                    (Expect.equal False
                        (grid { x = 2, y = 2 })
                    )
            , test "3,2 is NOT taken" <|
                \() ->
                    (Expect.equal False
                        (grid { x = 3, y = 2 })
                    )
            ]


getPositionTest : Test
getPositionTest =
    let
        -- GRID STATE:
        -- ===========
        --    0 1 2 3
        -- 0 |A|A|B|B|
        -- 1 |A|A| | |
        -- 2 | | | | |
        grid1 =
            [ (createItem 0 0 2 2)
            , (createItem 2 0 2 1)
            ]

        item width height =
            { width = width, height = height }
    in
        describe "getPosition"
            [ test "First position in grid" <|
                \() ->
                    (Expect.equal { x = 0, y = 0 }
                        (getPosition identity 4 [] (item 0 0) |> first)
                    )
              -- Expected result (N means New):
              -- ================
              --    0 1 2 3
              -- 0 |A|A|B|B|
              -- 1 |A|A|N|N|
              -- 2 | | |N|N|
            , test "Under previous" <|
                \() ->
                    (Expect.equal { x = 2, y = 1 }
                        (getPosition identity 4 grid1 (item 2 2) |> first)
                    )
              -- Expected result (N means New):
              -- ================
              --    0 1 2 3
              -- 0 |A|A|B|B|
              -- 1 |A|A| | |
              -- 2 |N|N|N| |
            , test "One new row" <|
                \() ->
                    (Expect.equal { x = 0, y = 2 }
                        (getPosition identity 4 grid1 (item 3 1) |> first)
                    )
              -- Expected result (N means New):
              -- ================
              --    0 1 2 3 4 5 6
              -- 0 |A|A|B|B|N|N|N|
              -- 1 |A|A| | |N|N|N|
              -- 2 | | | | | | | |
            , test "On same line" <|
                \() ->
                    (Expect.equal { x = 4, y = 0 }
                        (getPosition identity 7 grid1 (item 3 2) |> first)
                    )
            ]


solveTest : Test
solveTest =
    describe "solve"
        -- Expected result:
        -- ================
        --    0 1 2 3
        -- 0 |A|A|A|A|
        -- 1 |A|A|A|A|
        -- 2 |B|B|C|C|
        -- 3 | | |C|C|
        [ test "4 columns layout" <|
            \() ->
                (Expect.equal
                    [ ( { x = 0, y = 0 }, { width = 4, height = 2 } )
                    , ( { x = 0, y = 2 }, { width = 2, height = 1 } )
                    , ( { x = 2, y = 2 }, { width = 2, height = 2 } )
                    ]
                    (solve identity 4
                        [ { width = 4, height = 2 }
                        , { width = 2, height = 1 }
                        , { width = 2, height = 2 }
                        ]
                    )
                )
          -- Expected result:
          -- ================
          --    0 1
          -- 0 |A|A|
          -- 1 |B| |
          -- 2 |B| |
          -- 3 |C|C|
        , test "2 columns layout" <|
            \() ->
                (Expect.equal
                    [ ( { x = 0, y = 0 }, { width = 2, height = 1 } )
                    , ( { x = 0, y = 1 }, { width = 1, height = 2 } )
                    , ( { x = 0, y = 3 }, { width = 2, height = 1 } )
                    ]
                    (solve identity 2
                        [ { width = 2, height = 1 }
                        , { width = 1, height = 2 }
                        , { width = 2, height = 1 }
                        ]
                    )
                )
          -- Expected result:
          -- ================
          --    0 1 2 3
          -- 0 |A|A|B|B|
          -- 1 |C|C|B|B|
          -- 2 | | |B|B|
          -- 3 |D|D|D|D|
        , test "Collision with n-2" <|
            \() ->
                (Expect.equal
                    [ ( { x = 0, y = 0 }, { width = 2, height = 1 } )
                    , ( { x = 2, y = 0 }, { width = 2, height = 3 } )
                    , ( { x = 0, y = 1 }, { width = 2, height = 1 } )
                    , ( { x = 0, y = 3 }, { width = 4, height = 1 } )
                    ]
                    (solve identity 4
                        [ { width = 2, height = 1 }
                        , { width = 2, height = 3 }
                        , { width = 2, height = 1 }
                        , { width = 4, height = 1 }
                        ]
                    )
                )
        ]
