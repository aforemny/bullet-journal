module View.MonthlySpread exposing (..)

import Dict exposing (Dict)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar
import Type.Bullet as Bullet exposing (Bullet)


type alias Model =
    { year : Year
    , month : Month
    , items : Dict DayOfMonth String
    , bullets : List Bullet
    }


canonicalDate : Model -> ( Int, Int, Int )
canonicalDate model =
    ( model.year, model.month, 0 )


empty : Year -> Month -> Model
empty year month =
    { year = year
    , month = month
    , items = Dict.empty
    , bullets = []
    }


type alias Month =
    Int


type alias Year =
    Int


type alias DayOfMonth =
    Int


type Msg
    = ItemChanged DayOfMonth String
    | BulletChanged Index String


type alias Index =
    Int


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update lift msg model =
    case msg of
        ItemChanged dayOfMonth input ->
            ( { model | items = Dict.insert dayOfMonth input model.items }, Cmd.none )

        BulletChanged index input ->
            let
                numBullets =
                    List.length model.bullets
            in
                (model.bullets
                    |> \bullets ->
                        if numBullets < index + 1 then
                            bullets ++ List.repeat (index - numBullets + 1) (Bullet.Note { text = "" })
                        else
                            bullets
                )
                    |> List.indexedMap
                        (\otherIndex bullet ->
                            if otherIndex == index then
                                Bullet.Note { text = input }
                            else
                                bullet
                        )
                    |> \bullets ->
                        ( { model | bullets = bullets }, Cmd.none )


view : (Msg -> msg) -> Model -> Html msg
view lift model =
    Html.div
        [ Html.class "monthly-spread" ]
        [ Html.h1
            [ Html.class "monthly-spread__title"
            ]
            [ text (title model)
            ]
        , Html.div
            [ Html.class "monthly-spread__wrapper"
            ]
            [ Html.ol
                [ Html.class "monthly-spread__item-wrapper"
                ]
                (List.map
                    (\dayOfMonth ->
                        let
                            day =
                                Calendar.fromGregorian
                                    model.year
                                    model.month
                                    dayOfMonth

                            dayOfWeek =
                                Calendar.dayOfWeek day

                            item =
                                Dict.get dayOfMonth model.items
                                    |> Maybe.withDefault ""
                        in
                            Html.li
                                [ Html.class "monthly-spread__item"
                                ]
                                [ Html.span
                                    [ Html.class "monthly-spread__item__day-of-month"
                                    ]
                                    [ text (toString dayOfMonth)
                                    ]
                                , Html.span
                                    [ Html.class "monthly-spread__item__day-of-week"
                                    ]
                                    [ text
                                        (case dayOfWeek of
                                            Calendar.Monday ->
                                                "M"

                                            Calendar.Tuesday ->
                                                "T"

                                            Calendar.Wednesday ->
                                                "W"

                                            Calendar.Thursday ->
                                                "T"

                                            Calendar.Friday ->
                                                "F"

                                            Calendar.Saturday ->
                                                "S"

                                            Calendar.Sunday ->
                                                "S"
                                        )
                                    ]
                                , Html.input
                                    [ Html.class "monthly-spread__item__text"
                                    , Html.value item
                                    , Html.onInput (lift << ItemChanged dayOfMonth)
                                    ]
                                    []
                                ]
                    )
                    (List.range 1
                        (Calendar.monthLength
                            (Calendar.isLeapYear model.year)
                            model.month
                        )
                    )
                )
            , Html.ol
                [ Html.class "monthly-spread__bullet-wrapper"
                ]
                (List.indexedMap
                    (\index bullet ->
                        let
                            value =
                                case bullet of
                                    Bullet.Task { text } ->
                                        text

                                    Bullet.Event { text } ->
                                        text

                                    Bullet.Note { text } ->
                                        text
                        in
                            Bullet.view
                                { node = Html.li
                                , additionalAttributes =
                                    [ Html.class "monthly-spread__bullet"
                                    ]
                                }
                                bullet
                    )
                    (model.bullets ++ [ Bullet.Note { text = "" } ])
                )
            ]
        ]


title : Model -> String
title model =
    String.join " "
        [ case model.month of
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
