module DailySpread exposing (..)

import Dict exposing (Dict)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar


type alias Model =
    { year : Year
    , month : Month
    , dayOfMonth : DayOfMonth
    , bullets : List String
    }


empty : Year -> Month -> DayOfMonth -> Model
empty year month dayOfMonth =
    { year = year
    , month = month
    , dayOfMonth = dayOfMonth
    , bullets = []
    }


canonicalDate : Model -> ( Int, Int, Int )
canonicalDate model =
    ( model.year, model.month, model.dayOfMonth )


type alias Month =
    Int


type alias Year =
    Int


type alias DayOfMonth =
    Int


type Msg
    = BulletChanged Index String


type alias Index =
    Int


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update lift msg model =
    case msg of
        BulletChanged index input ->
            let
                numBullets =
                    List.length model.bullets
            in
                (model.bullets
                    |> \bullets ->
                        if numBullets < index + 1 then
                            bullets ++ List.repeat (index - numBullets + 1) ""
                        else
                            bullets
                )
                    |> List.indexedMap
                        (\otherIndex otherInput ->
                            if otherIndex == index then
                                input
                            else
                                otherInput
                        )
                    |> \bullets ->
                        ( { model | bullets = bullets }, Cmd.none )


view : (Msg -> msg) -> Model -> Html msg
view lift model =
    Html.div
        [ Html.class "daily-spread" ]
        [ Html.h1
            [ Html.class "daily-spread__title"
            ]
            [ text (title model)
            ]
        , Html.ol
            [ Html.class "daily-spread__bullet-wrapper"
            ]
            (List.indexedMap
                (\index bullet ->
                    Html.li
                        [ Html.class "daily-spread__bullet"
                        ]
                        [ Html.input
                            [ Html.class "daily-spread__bullet__native-control"
                            , Html.value bullet
                            , Html.onInput (lift << BulletChanged index)
                            ]
                            []
                        ]
                )
                (model.bullets ++ [ "" ])
            )
        ]


title : Model -> String
title model =
    String.join " "
        [ toString model.dayOfMonth
        , case model.month of
            1 ->
                "January"

            2 ->
                "February"

            3 ->
                "March"

            4 ->
                "April"

            5 ->
                "Mai"

            6 ->
                "June"

            7 ->
                "July"

            8 ->
                "August"

            9 ->
                "September"

            10 ->
                "October"

            11 ->
                "November"

            _ ->
                "December"
        , toString model.year
        ]
