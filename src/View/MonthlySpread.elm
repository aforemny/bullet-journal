module View.MonthlySpread exposing (..)

import Dict exposing (Dict)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Material
import Material.Button as Button
import Material.Toolbar as Toolbar
import Navigation
import Parse
import Task
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import Url exposing (Url)
import View


type alias Model msg =
    { mdc : Material.Model msg
    , monthlySpread : Maybe (Parse.Object MonthlySpread)
    , bullets : List (Parse.Object Bullet)
    , error : Maybe Parse.Error
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    , monthlySpread = Nothing
    , bullets = []
    , error = Nothing
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | MonthlySpreadResult (Result Parse.Error (Parse.Object MonthlySpread))
    | BulletsResult (Result Parse.Error (List (Parse.Object Bullet)))
    | NewBulletClicked



--    | ItemChanged MonthlySpread.DayOfMonth String
--    | BulletChanged Index String


type alias Index =
    Int


init :
    (Msg msg -> msg)
    -> View.Config msg
    -> Parse.ObjectId MonthlySpread
    -> Model msg
    -> ( Model msg, Cmd msg )
init lift viewConfig objectId model =
    ( defaultModel
    , Cmd.batch
        [ Material.init (lift << Mdc)
        , Task.attempt (lift << MonthlySpreadResult)
            (MonthlySpread.get viewConfig.parse objectId)
        , Task.attempt (lift << BulletsResult)
            (Bullet.get viewConfig.parse "MonthlySpread" objectId)
        ]
    )


subscriptions lift model =
    Material.subscriptions (lift << Mdc) model


update : (Msg msg -> msg) -> Msg msg -> Model msg -> ( Model msg, Cmd msg )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        MonthlySpreadResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        MonthlySpreadResult (Ok monthlySpread) ->
            ( { model | monthlySpread = Just monthlySpread }, Cmd.none )

        BulletsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        BulletsResult (Ok bullets) ->
            ( { model | bullets = bullets }, Cmd.none )

        NewBulletClicked ->
            ( model
            , model.monthlySpread
                |> Maybe.map (.objectId >> Bullet.anyObjectId)
                |> Maybe.map (Url.NewBullet "monthly-spread" "MonthlySpread")
                |> Maybe.map (Navigation.newUrl << Url.toString)
                |> Maybe.withDefault Cmd.none
            )



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
        monthlySpread_ =
            model.monthlySpread

        monthlySpread =
            Maybe.map MonthlySpread.fromParseObject monthlySpread_

        bullets =
            model.bullets
    in
        Html.div
            [ Html.class "monthly-spread" ]
            [ viewConfig.toolbar
                { additionalSections =
                    [ Toolbar.section
                        [ Toolbar.alignEnd
                        ]
                        [ Button.view (lift << Mdc)
                            "monthly-spread__new-bullet"
                            model.mdc
                            [ Button.ripple
                            , Button.onClick (lift NewBulletClicked)
                            ]
                            [ text "New bullet"
                            ]
                        ]
                    ]
                }
            , Html.h1
                [ Html.class "monthly-spread__title"
                ]
                [ monthlySpread
                    |> Maybe.map MonthlySpread.title
                    |> Maybe.withDefault ""
                    |> text
                ]
            , Html.div
                [ Html.class "monthly-spread__wrapper"
                ]
                [ Html.ol
                    [ Html.class "monthly-spread__item-wrapper"
                    ]
                    (monthlySpread
                        |> Maybe.map
                            (\monthlySpread ->
                                List.map
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
                                                ""
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
                        |> Maybe.withDefault [ text "" ]
                    )
                , Html.ol
                    [ Html.class "monthly-spread__bullet-wrapper"
                    ]
                    (List.indexedMap
                        (\index bullet ->
                            Bullet.view
                                { node = Html.li
                                , additionalAttributes =
                                    [ Html.class "monthly-spread__bullet"
                                    ]
                                }
                                (Bullet.fromParseObject bullet)
                        )
                        bullets
                    )
                ]
            ]
