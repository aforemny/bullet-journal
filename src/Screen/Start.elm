module Screen.Start exposing (Model, Msg(..), backlogCard, bulletClass, dailyBulletsCard, defaultModel, init, inputCard, monthlyBulletsCard, subscriptions, upcomingEventsCard, update, view, viewBullet)

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Material.Card exposing (card, cardBlock, cardConfig, cardPrimaryAction, cardPrimaryActionConfig)
import Material.Chip.Choice as Chip exposing (choiceChip, choiceChipConfig)
import Material.ChipSet exposing (choiceChipSet)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.List exposing (list, listConfig, listItem, listItemConfig, listItemGraphic, listItemPrimaryText, listItemSecondaryText, listItemText)
import Material.TextField exposing (textField, textFieldConfig)
import Material.TopAppBar as TopAppBar
import Parse
import Parse.Private.ObjectId as ObjectId
import Route exposing (Route)
import Screen
import Task exposing (Task)
import Time
import Time.Calendar.Gregorian as Calendar
import Time.Format.Locale as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.Bullet.Parser as Bullet
import Type.DailySpread as DailySpread exposing (DailySpread)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)


type alias Model =
    { bullets : List (Parse.Object Bullet)
    , input : String
    }


defaultModel : Model
defaultModel =
    { bullets = []
    , input = ""
    }


type Msg msg
    = NoOp
    | BulletsChanged (Result Parse.Error (List (Parse.Object Bullet)))
    | BulletClicked (Parse.Object Bullet)
    | InputChanged String
    | InputSubmitted String
    | BulletCreated (Result Parse.Error (Parse.Object Bullet))
    | BulletMarkedDone (Result Parse.Error (Parse.ObjectId Bullet))


init : (Msg msg -> msg) -> Screen.Config msg -> Model -> ( Model, Cmd msg )
init lift { today, parse } model =
    let
        ( year, month, dayOfMonth ) =
            Calendar.toGregorian today

        getBullets =
            Parse.toTask parse
                (Parse.query Bullet.decode
                    (Parse.emptyQuery "Bullet"
                     --                        |> (\query ->
                     --                                { query
                     --                                    | whereClause =
                     --                                        Parse.and
                     --                                            [ Parse.equalTo "year" (Encode.int year)
                     --                                            , Parse.equalTo "month" (Encode.int month)
                     --                                            ]
                     --                                }
                     --                           )
                    )
                )
    in
    ( defaultModel
    , Cmd.batch
        [ Task.attempt (lift << BulletsChanged) getBullets
        ]
    )


subscriptions : (Msg msg -> msg) -> Model -> Sub msg
subscriptions lift model =
    Sub.none


update :
    (Msg msg -> msg)
    -> Screen.Config msg
    -> Msg msg
    -> Model
    -> ( Model, Cmd msg )
update lift ({ today, parse } as viewConfig) msg model =
    case msg of
        BulletCreated (Err err) ->
            -- TODO:
            let
                _ =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        BulletCreated (Ok bullet) ->
            ( { model | bullets = bullet :: model.bullets }, Cmd.none )

        InputSubmitted input ->
            let
                ( year, month, dayOfMonth ) =
                    Calendar.toGregorian today

                bullet =
                    Bullet.parse input
                        |> (\bullet_ ->
                                { bullet_
                                    | year = Just year
                                    , month = Just month
                                    , dayOfMonth = Just dayOfMonth
                                }
                           )
            in
            ( { model | input = "" }
            , Task.attempt (lift << BulletCreated)
                (Bullet.create parse bullet
                    |> Task.andThen (Bullet.get parse << .objectId)
                )
            )

        InputChanged input ->
            ( { model | input = input }, Cmd.none )

        BulletMarkedDone (Err err) ->
            -- TODO:
            let
                _ =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        BulletMarkedDone (Ok objectId) ->
            let
                bullets =
                    model.bullets
                        |> List.filter ((/=) objectId << .objectId)
            in
            ( { model | bullets = bullets }, Cmd.none )

        BulletClicked bullet ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.EditBullet (Just bullet.objectId)))
            )

        --        BulletClicked bullet ->
        --            case bullet.state of
        --                Bullet.Task Bullet.Unchecked ->
        --                    ( model
        --                    , Task.attempt (lift << BulletMarkedDone)
        --                        (Bullet.update parse
        --                            bullet.objectId
        --                            (Bullet.fromParseObject bullet
        --                                |> (\bullet ->
        --                                        { bullet | state = Bullet.Task Bullet.Checked }
        --                                   )
        --                            )
        --                            |> Task.map (always bullet.objectId)
        --                        )
        --                    )
        --
        --                _ ->
        --                    ( model, Cmd.none )
        BulletsChanged (Err err) ->
            -- TODO:
            let
                _ =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        BulletsChanged (Ok bullets) ->
            ( { model | bullets = bullets }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : (Msg msg -> msg) -> Screen.Config msg -> Model -> List (Html msg)
view lift ({ today } as viewConfig) model =
    let
        sortedBullets =
            model.bullets
                |> List.filter
                    (\bullet ->
                        List.all identity
                            [ bullet.state /= Bullet.Note
                            , bullet.state /= Bullet.Task Bullet.Checked
                            ]
                    )
                |> List.sortBy
                    (\bullet ->
                        let
                            stateSort =
                                case bullet.state of
                                    Bullet.Task _ ->
                                        0

                                    Bullet.Note ->
                                        1

                                    Bullet.Event ->
                                        2

                            createdAtSort =
                                Time.posixToMillis bullet.createdAt
                        in
                        ( stateSort, createdAtSort )
                    )
    in
    [ viewConfig.topAppBar
        { title = "Overview"
        , menuIcon = Nothing
        , additionalSections = []
        }
    , Html.div
        [ class "start"
        , class "screen screen--scrollable"
        , viewConfig.fixedAdjust
        ]
        [ Html.div [ class "screen__wrapper" ]
            [ inputCard lift viewConfig model
            , dailyBulletsCard lift viewConfig model sortedBullets
            , monthlyBulletsCard lift viewConfig model sortedBullets
            , upcomingEventsCard lift viewConfig model sortedBullets
            , backlogCard lift viewConfig model sortedBullets
            ]
        ]
    ]


inputCard lift viewConfig model =
    Html.div []
        [ card
            { cardConfig
                | additionalAttributes =
                    [ class "start__input-card" ]
            }
            { blocks =
                cardPrimaryAction cardPrimaryActionConfig <|
                    [ cardBlock <|
                        Html.div []
                            [ textField
                                { textFieldConfig
                                    | placeholder = Just "Enter bullet"
                                    , fullwidth = True
                                    , value = Just model.input
                                    , additionalAttributes =
                                        [ Html.Events.on "keypress"
                                            (Decode.map (lift << InputSubmitted)
                                                (Decode.at [ "which" ] Decode.int
                                                    |> Decode.andThen
                                                        (\which ->
                                                            if which == 13 then
                                                                Html.Events.targetValue

                                                            else
                                                                Decode.fail ""
                                                        )
                                                )
                                            )
                                        , Html.Events.onInput (lift << InputChanged)
                                        ]
                                }
                            ]
                    ]
            , actions = Nothing
            }
        , let
            bullet =
                Bullet.parse model.input
          in
          choiceChipSet []
            (List.filterMap identity
                [ Just <|
                    choiceChip
                        choiceChipConfig
                        (case bullet.state of
                            Bullet.Task _ ->
                                "Task"

                            Bullet.Event ->
                                "Event"

                            Bullet.Note ->
                                "Note"
                        )
                , Just <|
                    choiceChip choiceChipConfig bullet.text
                ]
            )
        ]


dailyBulletsCard lift ({ today } as viewConfig) model sortedBullets =
    let
        bullets =
            sortedBullets
                |> List.filter
                    (\bullet ->
                        (bullet.year == Just year)
                            && (bullet.month == Just month)
                            && (bullet.dayOfMonth == Just dayOfMonth)
                    )

        title =
            String.join " "
                [ case Calendar.defaultTimeLocale of
                    Calendar.TimeLocale { months } ->
                        List.drop (month - 1) months
                            |> List.head
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault ""
                , String.fromInt dayOfMonth
                ]

        ( year, month, dayOfMonth ) =
            Calendar.toGregorian today
    in
    card
        { cardConfig
            | additionalAttributes = [ class "start__daily-bullets" ]
        }
        { blocks =
            [ cardBlock <|
                Html.div []
                    [ Html.h3 [ class "start__daily-bullets__title" ] [ text title ]
                    , list listConfig (List.map (viewBullet lift viewConfig model) bullets)
                    ]
            ]
        , actions = Nothing
        }


monthlyBulletsCard lift ({ today } as viewConfig) model sortedBullets =
    let
        bullets =
            sortedBullets
                |> List.filter
                    (\bullet ->
                        (bullet.year == Just year)
                            && (bullet.month == Just month)
                            && (Maybe.withDefault 0 bullet.dayOfMonth > dayOfMonth)
                    )

        title =
            String.join " "
                [ case Calendar.defaultTimeLocale of
                    Calendar.TimeLocale { months } ->
                        List.drop (month - 1) months
                            |> List.head
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault ""
                ]

        ( year, month, dayOfMonth ) =
            Calendar.toGregorian today
    in
    card
        { cardConfig
            | additionalAttributes = [ class "start__monthly-bullets" ]
        }
        { blocks =
            [ cardBlock <|
                Html.div []
                    [ Html.h3 [ class "start__daily-bullets__title" ] [ text title ]
                    , list listConfig (List.map (viewBullet lift viewConfig model) bullets)
                    ]
            ]
        , actions = Nothing
        }


upcomingEventsCard lift ({ today } as viewConfig) model sortedBullets =
    let
        bullets =
            sortedBullets
                |> List.filter
                    (\bullet ->
                        List.all identity
                            [ bullet.state == Bullet.Event
                            , Maybe.withDefault 0 bullet.year >= year
                            , Maybe.withDefault 0 bullet.month >= month
                            , Maybe.withDefault 0 bullet.dayOfMonth > dayOfMonth
                            ]
                    )

        title =
            "Upcoming events"

        ( year, month, dayOfMonth ) =
            Calendar.toGregorian today
    in
    card
        { cardConfig
            | additionalAttributes = [ class "start__monthly-bullets" ]
        }
        { blocks =
            [ cardBlock <|
                Html.div []
                    [ Html.h3 [ class "start__daily-bullets__title" ] [ text title ]
                    , list listConfig (List.map (viewBullet lift viewConfig model) bullets)
                    ]
            ]
        , actions = Nothing
        }


backlogCard lift ({ today } as viewConfig) model sortedBullets =
    let
        bullets =
            sortedBullets
                |> List.filter
                    (\bullet ->
                        List.all identity
                            [ bullet.state == Bullet.Task Bullet.Unchecked
                            , ( Maybe.withDefault 0 bullet.year
                              , Maybe.withDefault 0 bullet.month
                              , Maybe.withDefault 0 bullet.dayOfMonth
                              )
                                < ( year, month, dayOfMonth )
                            ]
                    )

        title =
            "Backlog"

        ( year, month, dayOfMonth ) =
            Calendar.toGregorian today
    in
    card
        { cardConfig
            | additionalAttributes =
                [ class "start__monthly-bullets" ]
        }
        { blocks =
            [ cardBlock <|
                Html.div []
                    [ Html.h3 [ class "start__daily-bullets__title" ] [ text title ]
                    , list listConfig (List.map (viewBullet lift viewConfig model) bullets)
                    ]
            ]
        , actions = Nothing
        }


bulletClass : Parse.Object Bullet -> String
bulletClass bullet =
    String.join " "
        [ "bullet"
        , case bullet.state of
            Bullet.Event ->
                "bullet--event"

            Bullet.Note ->
                "bullet--note"

            Bullet.Task Bullet.Unchecked ->
                "bullet--task bullet--task--unchecked"

            Bullet.Task Bullet.Checked ->
                "bullet--task bullet--task--checked"

            Bullet.Task Bullet.Migrated ->
                "bullet--task bullet--task--migrated"
        ]


viewBullet :
    (Msg msg -> msg)
    -> Screen.Config msg
    -> Model
    -> Parse.Object Bullet
    -> Html msg
viewBullet lift viewConfig model bullet =
    let
        date =
            case ( bullet.year, bullet.month, bullet.dayOfMonth ) of
                ( Just year, Just month, Just day ) ->
                    Just
                        (String.fromInt day
                            ++ ".\u{00A0}"
                            ++ String.fromInt month
                            ++ "\u{00A0}"
                            ++ String.fromInt year
                        )

                _ ->
                    Nothing
    in
    listItem
        { listItemConfig
            | additionalAttributes =
                [ class (bulletClass bullet)
                , Html.Events.onClick (lift (BulletClicked bullet))
                ]
        }
        [ listItemGraphic [] []
        , listItemText []
            [ text bullet.text
            , listItemSecondaryText [] [ text (Maybe.withDefault "–" date) ]
            ]
        ]
