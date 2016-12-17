module Main exposing (..)

-- Common

import List
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Random
import Result


-- Lib

import Grid


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- Model


type alias Item =
    { width : Int
    , height : Int
    }


type alias Model =
    { inRow : Int
    , items : List Item
    }



-- Generators


itemGenerator : Model -> Random.Generator Item
itemGenerator model =
    let
        dimGen =
            Random.int 1 <| round (toFloat model.inRow / 3)
    in
        Random.map2 (\w h -> Item w h) dimGen dimGen


itemsGenerator : Int -> Model -> Random.Generator (List Item)
itemsGenerator num model =
    Random.list num (itemGenerator model)


generateItems : Int -> Model -> Cmd Msg
generateItems num model =
    itemsGenerator num model
        |> Random.generate (\list -> AddItems list)



-- Update


init : ( Model, Cmd Msg )
init =
    let
        model =
            { inRow = 12
            , items = []
            }
    in
        model ! [ generateItems 40 model ]


type Msg
    = AddItems (List Item)
    | Generate Int
    | SetInRow Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddItems list ->
            { model | items = list } ! []

        Generate num ->
            model
                ! [ generateItems num model ]

        SetInRow num ->
            { model | inRow = num }
                ! [ generateItems (List.length model.items) model ]



-- Grid


grid : Model -> List ( Grid.Position, Item )
grid model =
    Grid.solve (\item -> { width = item.width, height = item.height }) model.inRow model.items



-- View


width : Int
width =
    600


toPx : Int -> Int -> String
toPx inRow num =
    let
        width_ =
            toFloat width

        inRow_ =
            toFloat inRow
    in
        toString (num * round (width_ / inRow_)) ++ "px"


itemStyle : Int -> Grid.Position -> Item -> Attribute Msg
itemStyle inRow position item =
    style
        [ ( "width", toPx inRow item.width )
        , ( "height", toPx inRow item.height )
        , ( "position", "absolute" )
        , ( "left", toPx inRow position.x )
        , ( "top", toPx inRow position.y )
        , ( "padding", "5px" )
        , ( "box-sizing", "border-box" )
        , ( "border-radius", "3px" )
        ]


itemView : Int -> ( Grid.Position, Item ) -> Html Msg
itemView inRow ( position, item ) =
    div [ itemStyle inRow position item ]
        [ div
            [ style
                [ ( "height", "100%" )
                , ( "background", "red" )
                ]
            ]
            []
        ]


containerStyle : Attribute Msg
containerStyle =
    style
        [ ( "width", toString width ++ "px" )
        , ( "margin", "0 auto" )
        , ( "position", "relative" )
        , ( "padding", "10px 0" )
        ]


gridView : Model -> Html Msg
gridView model =
    div [ containerStyle ]
        (List.map (itemView model.inRow) <| grid model)


view : Model -> Html Msg
view model =
    let
        argToInt : String -> Int
        argToInt str =
            Result.withDefault 1 (String.toInt str)

        desc : String
        desc =
            """
             This application is generating random items used for grid
             and uses Grid.solve to create layout.
             You can dynamixaly generate some number of new items and customize
             number of units (size 1) in row (that is de-facto scaling).
            """
    in
        div [ containerStyle ]
            [ div [ style [ ( "padding", "10px 0" ) ] ] [ text desc ]
            , label []
                [ text "number of items:"
                , input
                    [ type_ "range"
                    , Html.Attributes.max "100"
                    , Html.Attributes.min "1"
                    , step "1"
                    , onInput (\str -> Generate (argToInt str))
                    ]
                    []
                ]
            , label []
                [ text "Number of units in row"
                , input
                    [ type_ "range"
                    , Html.Attributes.max "30"
                    , Html.Attributes.min "1"
                    , step "1"
                    , onInput (\str -> SetInRow (argToInt str))
                    ]
                    []
                ]
            , (gridView model)
            ]
