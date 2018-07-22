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
import Types.Bullet as Bullet exposing (Bullet)


type alias Model =
    { year : Year
    , month : Month
    , dayOfMonth : DayOfMonth
    , bullets : List Bullet
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
                                [ Html.class "daily-spread__bullet"
                                ]
                            }
                            bullet
                )
                (model.bullets ++ [ Bullet.Note { text = "" } ])
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
