-- Example.elm
import Task
import Console exposing ((>>>))
import ElmTest exposing (..)

import GridBuilder

type alias Fixture = ( GridBuilder.GridPosition, { width : Int, height : Int } )

rangeTests : Test
rangeTests =
  suite "isInRange"
          [ test "2 is in range <1,5]" (assertEqual True
                                          (GridBuilder.isInRange 1 5 2))
          , test "7 is not in range 1..6]" (assertEqual False
                                              (GridBuilder.isInRange 1 6 7))
          , test "1.01 is in range <1, 2]" (assertEqual True
                                              (GridBuilder.isInRange 1 2 1))
          , test "1 is in range <1, 2]" (assertEqual True
                                           (GridBuilder.isInRange 1 2 1))
          , test "2 is not in range <1,2]" (assertEqual False
                                              (GridBuilder.isInRange 1 2 2)) ]

createItem : Int -> Int -> Int -> Int -> Fixture
createItem x y width height =
   ( { x = x, y = y },  { width = width, height = height } )

isTakenTests : Test
isTakenTests =
  let
    -- GRID STATE:
    -- ===========
    -- X  0 1 2 3
    -- 0 |A|A|A|A|
    -- 1 |B|B| | |
    -- 2 |B|B| | |
    state = [ (createItem 0 0 4 1)
            , (createItem 0 1 2 2) ]
    grid = GridBuilder.isTaken state
  in
    suite "isTaken"
            [ test "0,0 is taken" (assertEqual True
                         (grid { x = 0, y = 0 }))
            , test "1,0 is taken" (assertEqual True
                         (grid { x = 1, y = 0 }))
            , test "2,0 is taken" (assertEqual True
                         (grid { x = 2, y = 0 }))
            , test "3,0 is taken" (assertEqual True
                         (grid { x = 3, y = 0 }))
            , test "4,0 is NOT taken (out of grid!)" (assertEqual False
                         (grid { x = 4, y = 0 }))
            , test "0,1 is taken" (assertEqual True
                         (grid { x = 0, y = 1 }))
            , test "1,1 is taken" (assertEqual True
                         (grid { x = 1, y = 1 }))
            , test "2,1 is NOT taken" (assertEqual False
                         (grid { x = 2, y = 1 }))
            , test "3,1 is NOT taken" (assertEqual False
                         (grid { x = 3, y = 1 }))
            , test "0,2 is taken" (assertEqual True
                         (grid { x = 0, y = 2 }))
            , test "1,2 is taken" (assertEqual True
                         (grid { x = 1, y = 2 }))
            , test "2,2 is NOT taken" (assertEqual False
                         (grid { x = 2, y = 2 }))
            , test "3,2 is NOT taken" (assertEqual False
                         (grid { x = 3, y = 2 }))
            ]

getPositionTests : Test
getPositionTests =
  let
    -- GRID STATE:
    -- ===========
    --    0 1 2 3
    -- 0 |A|A|B|B|
    -- 1 |A|A| | |
    -- 2 | | | | |
    grid1 =
      [ (createItem 0 0 2 2)
      , (createItem 2 0 2 1) ]
    item width height =
      { width = width, height = height }
  in
    suite "getPosition"
            [ test "First position in grid" (assertEqual { x = 0, y = 0 }
                                               (GridBuilder.getPosition 4 [] (item 0 0) |> fst))
            -- Expected result (N is New):
            -- ================
            --    0 1 2 3
            -- 0 |A|A|B|B|
            -- 1 |A|A|N|N|
            -- 2 | | |N|N|
            , test "" (assertEqual { x = 2, y = 1 }
                       (GridBuilder.getPosition 4 grid1 (item 2 2) |> fst ))

            -- Expected result (N is New):
            -- ================
            --    0 1 2 3
            -- 0 |A|A|B|B|
            -- 1 |A|A| | |
            -- 2 |N|N|N| |
            , test "" (assertEqual { x = 0, y = 2 }
                       (GridBuilder.getPosition 4 grid1 (item 3 1) |> fst))

            -- Expected result (N is New):
            -- ================
            --    0 1 2 3 4 5 6
            -- 0 |A|A|B|B|N|N|N|
            -- 1 |A|A| | |N|N|N|
            -- 2 | | | | | | | |
            , test "" (assertEqual { x = 4, y = 0 }
                      (GridBuilder.getPosition 7 grid1 (item 3 2) |> fst)) ]

getPositionsTests : Test
getPositionsTests =
  suite "getPositions"
          -- Expected result (N is New):
          --    ================
          --       0 1 2 3
          --    0 |A|A|A|A|
          --    1 |A|A|A|A|
          --    2 |B|B|C|C|
          --    3 | | |C|C|
          [ test "" (assertEqual
                       [ ( { x = 0, y = 0 }, { width = 4, height = 2 } )
                       , ( { x = 0, y = 2 }, { width = 2, height = 1 } )
                       , ( { x = 2, y = 2 }, { width = 2, height = 2 } ) ]
                       (GridBuilder.getPositions 4
                          [ { width = 4, height = 2 }
                          , { width = 2, height = 1 }
                          , { width = 2, height = 2 } ]))
          -- Expected result (N is New):
          -- ================
          --    0 1
          -- 0 |A|A|
          -- 1 |B| |
          -- 2 |B| |
          -- 3 |C|C|
          , test "" (assertEqual
                       [ ( { x = 0, y = 0 }, { width = 2, height = 1 } )
                       , ( { x = 0, y = 1 }, { width = 1, height = 2 } )
                       , ( { x = 0, y = 3 }, { width = 2, height = 1 } ) ]
                       (GridBuilder.getPositions 2
                          [ { width = 2, height = 1 }
                          , { width = 1, height = 2 }
                          , { width = 2, height = 1 } ])) ]

port runner : Signal (Task.Task x ())
port runner =
  Console.run (consoleRunner (suite "All tests"
                                [ rangeTests
                                , isTakenTests
                                , getPositionTests
                                , getPositionsTests ]))
