module View.MonthlySpread exposing (..)

import Dict exposing (Dict)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Material
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import View


type alias Model msg =
    { mdc : Material.Model msg
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    }


type Msg msg
    = Mdc (Material.Msg msg)



--    | ItemChanged MonthlySpread.DayOfMonth String
--    | BulletChanged Index String


type alias Index =
    Int


init lift =
  Cmd.none


subscriptions lift model =
  Material.subscriptions (lift << Mdc) model


update : (Msg msg -> msg) -> Msg msg -> Model msg -> ( Model msg, Cmd msg )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model



--        ItemChanged dayOfMonth input ->
--            ( { model | items = Dict.insert dayOfMonth input monthlySpread.items }, Cmd.none )
--
--        BulletChanged index input ->
--            let
--                numBullets =
--                    List.length monthlySpread.bullets
--            in
--                (monthlySpread.bullets
--                    |> \bullets ->
--                        if numBullets < index + 1 then
--                            bullets ++ List.repeat (index - numBullets + 1) (Bullet.Note { text = "" })
--                        else
--                            bullets
--                )
--                    |> List.indexedMap
--                        (\otherIndex bullet ->
--                            if otherIndex == index then
--                                Bullet.Note { text = input }
--                            else
--                                bullet
--                        )
--                    |> \bullets ->
--                        ( { model | bullets = bullets }, Cmd.none )


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    let
        monthlySpread =
            MonthlySpread.empty 2018 7
    in
        Html.div
            [ Html.class "monthly-spread" ]
            [ viewConfig.toolbar
                { additionalSections = []
                }
            , Html.h1
                [ Html.class "monthly-spread__title"
                ]
                [ text (MonthlySpread.title monthlySpread)
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
                                        monthlySpread.year
                                        monthlySpread.month
                                        dayOfMonth

                                dayOfWeek =
                                    Calendar.dayOfWeek day

                                item =
                                    Dict.get dayOfMonth monthlySpread.items
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
                                          -- , Html.onInput (lift << ItemChanged dayOfMonth)
                                        ]
                                        []
                                    ]
                        )
                        (List.range 1
                            (Calendar.monthLength
                                (Calendar.isLeapYear monthlySpread.year)
                                monthlySpread.month
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
                        (monthlySpread.bullets ++ [ Bullet.Note { text = "" } ])
                    )
                ]
            ]
