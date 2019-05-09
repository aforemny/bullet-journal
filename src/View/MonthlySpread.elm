module View.MonthlySpread exposing (Index, Model, Msg(..), dayView, defaultModel, init, subscriptions, update, view)

import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes as Html
import Html.Events as Html
import Json.Decode as Decode exposing (Decoder)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options exposing (cs, css, styled, when)
import Material.TextField as TextField
import Material.Toolbar as Toolbar
import Parse
import Route exposing (Route)
import Task
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.Day as Day exposing (Day)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import View


type alias Model msg =
    { mdc : Material.Model msg
    , monthlySpread : Maybe (Parse.Object MonthlySpread)
    , bullets : List (Parse.Object Bullet)
    , days : Dict ( Day.Month, Day.DayOfMonth ) Day
    , error : Maybe Parse.Error
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    , monthlySpread = Nothing
    , bullets = []
    , days = Dict.empty
    , error = Nothing
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | MonthlySpreadResult (Result Parse.Error (Parse.Object MonthlySpread))
    | BulletsResult (Result Parse.Error (List (Parse.Object Bullet)))
    | DaysResult (Result Parse.Error (List (Parse.Object Day)))
    | NewBulletClicked
    | EditClicked
    | DayChanged Day.Month Day.DayOfMonth String
    | DayResult Day.Month Day.DayOfMonth (Result Parse.Error (Result Day.Update Day.Create))
    | BackClicked
    | BulletClicked (Parse.ObjectId Bullet)


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
            (Bullet.getOf viewConfig.parse "MonthlySpread" objectId)
        ]
    )


subscriptions lift model =
    Material.subscriptions (lift << Mdc) model


update :
    (Msg msg -> msg)
    -> View.Config msg
    -> Msg msg
    -> Model msg
    -> ( Model msg, Cmd msg )
update lift viewConfig msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        MonthlySpreadResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        MonthlySpreadResult (Ok monthlySpread) ->
            ( { model | monthlySpread = Just monthlySpread }
            , Task.attempt (lift << DaysResult)
                (Day.list viewConfig.parse monthlySpread.month)
            )

        BulletsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        BulletsResult (Ok bullets) ->
            ( { model | bullets = bullets }, Cmd.none )

        NewBulletClicked ->
            ( model, Browser.Navigation.pushUrl viewConfig.key (Route.toString (Route.EditBullet Nothing)) )

        EditClicked ->
            ( model
            , model.monthlySpread
                |> Maybe.map (Route.EditMonthlySpread << .objectId)
                |> Maybe.map (Browser.Navigation.pushUrl viewConfig.key << Route.toString)
                |> Maybe.withDefault Cmd.none
            )

        DaysResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DaysResult (Ok days_) ->
            let
                days =
                    days_
                        |> List.map
                            (\day ->
                                ( ( day.month, day.dayOfMonth ), Day.fromParseObject day )
                            )
                        |> Dict.fromList
            in
            ( { model | days = days }, Cmd.none )

        DayChanged month dayOfMonth text ->
            ( { model
                | days =
                    Dict.insert ( month, dayOfMonth )
                        { month = month
                        , dayOfMonth = dayOfMonth
                        , text = text
                        }
                        model.days
              }
            , Task.attempt (lift << DayResult month dayOfMonth)
                (Day.createOrUpdate viewConfig.parse
                    { month = month
                    , dayOfMonth = dayOfMonth
                    , text = text
                    }
                )
            )

        DayResult month dayOfMonth (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DayResult month dayOfMonth (Ok (Ok { objectId, createdAt })) ->
            ( model, Cmd.none )

        DayResult month dayOfMonth (Ok (Err { updatedAt })) ->
            ( model, Cmd.none )

        BackClicked ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString Route.Index)
            )

        BulletClicked bulletId ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.EditBullet (Just bulletId)))
            )


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    let
        monthlySpread_ =
            model.monthlySpread

        maybeMonthlySpread =
            Maybe.map MonthlySpread.fromParseObject monthlySpread_

        title =
            maybeMonthlySpread
                |> Maybe.map MonthlySpread.title
                |> Maybe.withDefault ""

        bullets =
            model.bullets
    in
    Html.div
        [ Html.class "monthly-spread" ]
        [ viewConfig.toolbar
            { title =
                title
            , menuIcon =
                Icon.view
                    [ Toolbar.menuIcon
                    , Options.onClick (lift BackClicked)
                    ]
                    "arrow_back"
            , additionalSections =
                [ Toolbar.section
                    [ Toolbar.alignEnd
                    ]
                    [ Button.view (lift << Mdc)
                        "monthly-spread__edit-monthly-spread"
                        model.mdc
                        [ Button.ripple
                        , Button.onClick (lift EditClicked)
                        ]
                        [ text "Edit"
                        ]
                    , Button.view (lift << Mdc)
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
        , Card.view
            [ cs "monthly-spread__wrapper"
            ]
            [ Html.div
                [ Html.class "monthly-spread__primary" ]
                [ Html.div
                    [ Html.class "monthly-spread__title" ]
                    [ text title ]
                , Html.div
                    [ Html.class "monthly-spread__subtitle" ]
                    [ text "The Monthly Log helps you organize your month. It consists of a calendar and a task list." ]
                ]
            , Html.div
                [ Html.class "monthly-spread__content-wrapper" ]
                [ Html.ol
                    [ Html.class "monthly-spread__days-wrapper" ]
                    (maybeMonthlySpread
                        |> Maybe.map
                            (\monthlySpread ->
                                List.map
                                    (\dayOfMonth ->
                                        dayView lift monthlySpread dayOfMonth model
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
                , Lists.ol (lift << Mdc)
                    -- TODO: id
                    ""
                    model.mdc
                    [ cs "monthly-spread__bullets-wrapper"
                    ]
                    (List.map
                        (\bullet ->
                            Bullet.view
                                { additionalOptions =
                                    [ cs "monthly-spread__bullet"
                                    , Options.onClick
                                        (lift (BulletClicked bullet.objectId))
                                    ]
                                }
                                (Bullet.fromParseObject bullet)
                        )
                        bullets
                    )
                ]
            ]
        ]


dayView lift monthlySpread dayOfMonth model =
    let
        day =
            Calendar.fromGregorian
                monthlySpread.year
                monthlySpread.month
                dayOfMonth

        dayOfWeek =
            Calendar.dayOfWeek day

        value =
            model.days
                |> Dict.get ( monthlySpread.month, dayOfMonth )
                |> Maybe.map .text
                |> Maybe.withDefault ""
    in
    Html.li
        [ Html.class "monthly-spread__day"
        ]
        [ Html.span
            [ Html.class "monthly-spread__day__day-of-month"
            ]
            [ text (String.fromInt dayOfMonth)
            ]
        , Html.span
            [ Html.class "monthly-spread__day__day-of-week"
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
        , TextField.view (lift << Mdc)
            ("monthly-spread__day__text-"
                ++ String.fromInt monthlySpread.month
                ++ "-"
                ++ String.fromInt dayOfMonth
            )
            model.mdc
            [ TextField.value value
            , Options.onInput (lift << DayChanged monthlySpread.month dayOfMonth)
            , TextField.fullwidth
            ]
            []
        ]
