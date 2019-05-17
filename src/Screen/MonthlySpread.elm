module Screen.MonthlySpread exposing (Index, Model, Msg(..), dayView, defaultModel, init, subscriptions, update, view)

import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Material.Button exposing (buttonConfig, textButton)
import Material.Card exposing (card, cardBlock, cardConfig)
import Material.Icon exposing (icon, iconConfig)
import Material.List exposing (list, listConfig)
import Material.TextField exposing (textField, textFieldConfig)
import Material.TopAppBar as TopAppBar
import Parse
import Route exposing (Route)
import Screen
import Task
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.Day as Day exposing (Day)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)


type alias Model =
    { monthlySpread : Maybe (Parse.Object MonthlySpread)
    , bullets : List (Parse.Object Bullet)
    , days : Dict ( Day.Month, Day.DayOfMonth ) Day
    , error : Maybe Parse.Error
    }


defaultModel : Model
defaultModel =
    { monthlySpread = Nothing
    , bullets = []
    , days = Dict.empty
    , error = Nothing
    }


type Msg msg
    = MonthlySpreadResult (Result Parse.Error (Parse.Object MonthlySpread))
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
    -> Screen.Config msg
    -> Parse.ObjectId MonthlySpread
    -> Model
    -> ( Model, Cmd msg )
init lift viewConfig objectId model =
    ( defaultModel
    , Cmd.batch
        [ Task.attempt (lift << MonthlySpreadResult)
            (MonthlySpread.get viewConfig.parse objectId)
        , Task.attempt (lift << BulletsResult)
            (Bullet.getOf viewConfig.parse "MonthlySpread" objectId)
        ]
    )


subscriptions lift model =
    Sub.none


update :
    (Msg msg -> msg)
    -> Screen.Config msg
    -> Msg msg
    -> Model
    -> ( Model, Cmd msg )
update lift viewConfig msg model =
    case msg of
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
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString Route.TableOfContent)
            )

        BulletClicked bulletId ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.EditBullet (Just bulletId)))
            )


view : (Msg msg -> msg) -> Screen.Config msg -> Model -> List (Html msg)
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
    [ viewConfig.topAppBar
        { title = title
        , menuIcon =
            icon
                { iconConfig
                    | additionalAttributes =
                        [ TopAppBar.navigationIcon
                        , Html.Events.onClick (lift BackClicked)
                        ]
                }
                "arrow_back"
        , additionalSections =
            [ TopAppBar.section [ TopAppBar.alignEnd ]
                [ textButton
                    { buttonConfig
                        | onClick = Just (lift EditClicked)
                    }
                    "Edit"
                , textButton
                    { buttonConfig
                        | onClick = Just (lift NewBulletClicked)
                    }
                    "New bullet"
                ]
            ]
        }
    , Html.div
        [ class "monthly-spread"
        , viewConfig.fixedAdjust
        ]
        [ card
            { cardConfig
                | additionalAttributes = [ class "monthly-spread__wrapper" ]
            }
            { blocks =
                [ cardBlock <|
                    Html.div []
                        [ Html.div
                            [ class "monthly-spread__primary" ]
                            [ Html.div
                                [ class "monthly-spread__title" ]
                                [ text title ]
                            , Html.div
                                [ class "monthly-spread__subtitle" ]
                                [ text "The Monthly Log helps you organize your month. It consists of a calendar and a task list." ]
                            ]
                        , Html.div
                            [ class "monthly-spread__content-wrapper" ]
                            [ Html.ol
                                [ class "monthly-spread__days-wrapper" ]
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
                            , list
                                { listConfig
                                    | additionalAttributes =
                                        [ class "monthly-spread__bullets-wrapper" ]
                                }
                                (List.map
                                    (\bullet ->
                                        Bullet.view
                                            { additionalOptions =
                                                [ class "monthly-spread__bullet"
                                                , Html.Events.onClick
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
            , actions = Nothing
            }
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
        [ class "monthly-spread__day"
        ]
        [ Html.span
            [ class "monthly-spread__day__day-of-month"
            ]
            [ text (String.fromInt dayOfMonth)
            ]
        , Html.span
            [ class "monthly-spread__day__day-of-week"
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
        , textField
            { textFieldConfig
                | value = Just value
                , onInput = Just (lift << DayChanged monthlySpread.month dayOfMonth)
                , fullwidth = True
            }
        ]
