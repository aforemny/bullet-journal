module Screen.EditBullet exposing (Model, Msg(..), Tipe(..), bulletDelete, bulletForm, defaultModel, deleteBullet, init, subscriptions, update, view)

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Material.Button exposing (buttonConfig, raisedButton, textButton)
import Material.Checkbox exposing (checkbox, checkboxConfig)
import Material.Dialog exposing (acceptButton, cancelButton, dialog, dialogConfig)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.Select exposing (filledSelect, selectConfig, selectOption, selectOptionConfig)
import Material.TextField exposing (textField, textFieldConfig)
import Material.TopAppBar as TopAppBar
import Parse
import Parse.Private.ObjectId as ObjectId
import Parse.Private.Pointer as Pointer
import Route exposing (Route)
import Screen
import Task
import Time
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
import Type.DailySpread as DailySpread exposing (DailySpread)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)


type alias Model =
    { referringUrl : Route
    , bulletId : Maybe (Parse.ObjectId Bullet)
    , bullet : Maybe (Parse.Object Bullet)
    , spreadId : Maybe (Parse.ObjectId Bullet.Any)
    , tipe : Tipe
    , taskState : Bullet.TaskState
    , text : String
    , year : String
    , month : String
    , dayOfMonth : String
    , error : Maybe Parse.Error
    , showConfirmDeleteDialog : Bool
    , dailySpreads : List (Parse.Object DailySpread)
    , monthlySpreads : List (Parse.Object MonthlySpread)
    , collectionSpreads : List (Parse.Object CollectionSpread)
    }


type Tipe
    = Task
    | Event
    | Note


defaultModel :
    Route
    -> Maybe (Parse.ObjectId Bullet)
    -> Model
defaultModel referringUrl bulletId =
    { referringUrl = referringUrl
    , bulletId = bulletId
    , bullet = Nothing
    , spreadId = Nothing
    , tipe = Note
    , taskState = Bullet.Unchecked
    , text = ""
    , error = Nothing
    , year = ""
    , month = ""
    , dayOfMonth = ""
    , showConfirmDeleteDialog = False
    , dailySpreads = []
    , monthlySpreads = []
    , collectionSpreads = []
    }


type Msg msg
    = BulletResult (Result Parse.Error (Parse.Object Bullet))
    | SpreadChanged (Maybe (Parse.ObjectId Bullet.Any))
    | TipeChanged Tipe
    | TaskStateChanged Bullet.TaskState
    | TextChanged String
    | SaveClicked
    | CreateResult (Result Parse.Error { objectId : Parse.ObjectId Bullet, createdAt : Time.Posix })
    | UpdateResult (Result Parse.Error { updatedAt : Time.Posix })
    | CancelClicked
    | BackClicked
    | DeleteClicked
    | ConfirmDeleteDialogClosed
    | CancelDeleteClicked
    | ConfirmDeleteClicked
    | DeleteResult (Result Parse.Error {})
    | DayOfMonthChanged String
    | MonthChanged String
    | YearChanged String
    | DailySpreadsChanged (Result Parse.Error (List (Parse.Object DailySpread)))
    | MonthlySpreadsChanged (Result Parse.Error (List (Parse.Object MonthlySpread)))
    | CollectionSpreadsChanged (Result Parse.Error (List (Parse.Object CollectionSpread)))


init :
    (Msg msg -> msg)
    -> Screen.Config msg
    -> Route
    -> Maybe (Parse.ObjectId Bullet)
    -> Maybe Model
    -> ( Model, Cmd msg )
init lift viewConfig referringUrl bulletId model =
    ( defaultModel referringUrl bulletId
    , Cmd.batch
        [ bulletId
            |> Maybe.map (Bullet.get viewConfig.parse)
            |> Maybe.map (Task.attempt (lift << BulletResult))
            |> Maybe.withDefault Cmd.none
        , Parse.send viewConfig.parse
            (lift << DailySpreadsChanged)
            (Parse.query DailySpread.decode (Parse.emptyQuery "DailySpread"))
        , Parse.send viewConfig.parse
            (lift << MonthlySpreadsChanged)
            (Parse.query MonthlySpread.decode (Parse.emptyQuery "MonthlySpread"))
        , Parse.send viewConfig.parse
            (lift << CollectionSpreadsChanged)
            (Parse.query CollectionSpread.decode (Parse.emptyQuery "CollectionSpread"))
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
update lift viewConfig msg model =
    case msg of
        DayOfMonthChanged dayOfMonth ->
            ( { model | dayOfMonth = dayOfMonth }, Cmd.none )

        MonthChanged month ->
            ( { model | month = month }, Cmd.none )

        YearChanged year ->
            ( { model | year = year }, Cmd.none )

        SpreadChanged spreadId ->
            ( { model | spreadId = spreadId }, Cmd.none )

        TipeChanged tipe ->
            ( { model
                | tipe = tipe
                , taskState = Bullet.Unchecked
              }
            , Cmd.none
            )

        TaskStateChanged taskState ->
            ( { model | taskState = taskState }, Cmd.none )

        TextChanged text ->
            ( { model | text = text }, Cmd.none )

        SaveClicked ->
            let
                state =
                    case model.tipe of
                        Task ->
                            Bullet.Task model.taskState

                        Event ->
                            Bullet.Event

                        Note ->
                            Bullet.Note

                bullet =
                    { spreadClass = Nothing
                    , spreadId = model.spreadId
                    , state = state
                    , text = model.text
                    , year = String.toInt model.year
                    , month = String.toInt model.month
                    , dayOfMonth = String.toInt model.dayOfMonth
                    }
            in
            ( model
            , model.bulletId
                |> Maybe.map
                    (\bulletId ->
                        Task.attempt (lift << UpdateResult)
                            (Bullet.update viewConfig.parse bulletId bullet)
                    )
                |> Maybe.withDefault
                    (Task.attempt (lift << CreateResult)
                        (Bullet.create viewConfig.parse bullet)
                    )
            )

        UpdateResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        UpdateResult (Ok _) ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString model.referringUrl)
            )

        CreateResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        CreateResult (Ok _) ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString model.referringUrl)
            )

        CancelClicked ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString model.referringUrl)
            )

        BackClicked ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString model.referringUrl)
            )

        DeleteClicked ->
            ( { model | showConfirmDeleteDialog = True }, Cmd.none )

        ConfirmDeleteDialogClosed ->
            ( { model | showConfirmDeleteDialog = False }, Cmd.none )

        CancelDeleteClicked ->
            ( { model | showConfirmDeleteDialog = False }, Cmd.none )

        ConfirmDeleteClicked ->
            ( { model | showConfirmDeleteDialog = False }
            , model.bulletId
                |> Maybe.map (Bullet.delete viewConfig.parse)
                |> Maybe.map (Task.attempt (lift << DeleteResult))
                |> Maybe.withDefault Cmd.none
            )

        DeleteResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DeleteResult (Ok _) ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString model.referringUrl)
            )

        BulletResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        BulletResult (Ok bullet) ->
            ( { model
                | bullet = Just bullet
                , tipe =
                    case bullet.state of
                        Bullet.Event ->
                            Event

                        Bullet.Note ->
                            Note

                        Bullet.Task _ ->
                            Task
                , taskState =
                    case bullet.state of
                        Bullet.Event ->
                            Bullet.Unchecked

                        Bullet.Note ->
                            Bullet.Unchecked

                        Bullet.Task taskState ->
                            taskState
                , text = bullet.text
                , year = Maybe.withDefault "" (Maybe.map String.fromInt bullet.year)
                , month = Maybe.withDefault "" (Maybe.map String.fromInt bullet.month)
                , dayOfMonth =
                    Maybe.withDefault "" (Maybe.map String.fromInt bullet.dayOfMonth)
              }
            , Cmd.none
            )

        DailySpreadsChanged (Err _) ->
            ( model, Cmd.none )

        DailySpreadsChanged (Ok dailySpreads) ->
            ( { model | dailySpreads = dailySpreads }, Cmd.none )

        MonthlySpreadsChanged (Err _) ->
            ( model, Cmd.none )

        MonthlySpreadsChanged (Ok monthlySpreads) ->
            ( { model | monthlySpreads = monthlySpreads }, Cmd.none )

        CollectionSpreadsChanged (Err _) ->
            ( model, Cmd.none )

        CollectionSpreadsChanged (Ok collectionSpreads) ->
            ( { model | collectionSpreads = collectionSpreads }, Cmd.none )


view : (Msg msg -> msg) -> Screen.Config msg -> Model -> List (Html msg)
view lift viewConfig model =
    [ viewConfig.topAppBar
        { title =
            model.bulletId
                |> Maybe.map ((++) "Edit " << ObjectId.toString)
                |> Maybe.withDefault "New bullet"
        , menuIcon =
            Just <|
                iconButton
                    { iconButtonConfig
                        | onClick = Just (lift BackClicked)
                        , additionalAttributes =
                            [ TopAppBar.navigationIcon ]
                    }
                    "arrow_back"
        , additionalSections = []
        }
    , Html.div
        [ class "screen edit-bullet"
        , viewConfig.fixedAdjust
        , case model.tipe of
            Task ->
                class "edit-bullet--tipe-task"

            Event ->
                class "edit-bullet--tipe-event"

            Note ->
                class "edit-bullet--tipe-note"
        ]
        [ Html.div [ class "screen__wrapper" ]
            [ Html.div [ class "edit-bullet__wrapper" ]
                [ bulletForm lift model
                , bulletDelete lift model
                ]
            ]
        ]
    ]


bulletForm lift model =
    let
        formButtons =
            Html.div
                [ class "edit-bullet__buttons-wrapper" ]
                [ saveButton

                -- , cancelButton
                ]

        bulletTipe =
            Html.div
                [ class "edit-bullet__tipe-wrapper" ]
                [ filledSelect
                    { selectConfig
                        | label = "Type"
                        , value =
                            case model.tipe of
                                Task ->
                                    Just "task"

                                Event ->
                                    Just "event"

                                Note ->
                                    Just "note"
                        , onChange =
                            Just
                                (\value ->
                                    lift <|
                                        TipeChanged <|
                                            case value of
                                                "task" ->
                                                    Task

                                                "event" ->
                                                    Event

                                                _ ->
                                                    Note
                                )
                    }
                    [ selectOption { selectOptionConfig | value = "task" }
                        [ text "Task" ]
                    , selectOption { selectOptionConfig | value = "event" }
                        [ text "Event" ]
                    , selectOption { selectOptionConfig | value = "note" }
                        [ text "Note" ]
                    ]
                , Html.div
                    [ class "edit-bullet__task-state-wrapper"
                    ]
                    [ filledSelect
                        { selectConfig
                            | label = "Type"
                            , value =
                                case model.taskState of
                                    Bullet.Checked ->
                                        Just "checked"

                                    Bullet.Unchecked ->
                                        Just "unchecked"

                                    Bullet.Migrated ->
                                        Nothing
                            , onChange =
                                Just
                                    (\value ->
                                        lift <|
                                            TaskStateChanged <|
                                                case value of
                                                    "checked" ->
                                                        Bullet.Checked

                                                    "unchecked" ->
                                                        Bullet.Unchecked

                                                    _ ->
                                                        Bullet.Migrated
                                    )
                        }
                        [ selectOption { selectOptionConfig | value = "unchecked" }
                            [ text "Unchecked" ]
                        , selectOption { selectOptionConfig | value = "checked" }
                            [ text "Checked" ]
                        , selectOption { selectOptionConfig | value = "migrated" }
                            [ text "Migrated" ]
                        ]
                    ]
                ]

        bulletText =
            Html.div
                [ class "edit-bullet__text" ]
                [ Html.div
                    [ class "edit-bullet__headline" ]
                    [ text "Text"
                    ]
                , Html.div
                    [ class "edit-bullet__text-wrapper" ]
                    [ textField
                        { textFieldConfig
                            | fullwidth = True
                            , value = Just model.text
                            , onInput = Just (lift << TextChanged)
                        }
                    ]
                ]

        bulletSpread =
            Html.div
                [ class "edit-bullet__spread" ]
                [ Html.div
                    [ class "edit-bullet__headline" ]
                    [ text "Spread" ]
                , Html.div
                    [ class "edit-bullet__spread-wrapper" ]
                    [ filledSelect
                        { selectConfig
                            | label = "Spread"
                            , onChange =
                                Just
                                    (\value ->
                                        lift
                                            (SpreadChanged
                                                (if value /= "" then
                                                    Just (ObjectId.fromString value)

                                                 else
                                                    Nothing
                                                )
                                            )
                                    )
                            , value =
                                Just
                                    (Maybe.withDefault ""
                                        (Maybe.map ObjectId.toString model.spreadId)
                                    )
                        }
                        (List.concat
                            [ [ selectOption selectOptionConfig [ text "" ] ]
                            , List.map
                                (\spread ->
                                    selectOption
                                        { selectOptionConfig
                                            | value = ObjectId.toString spread.objectId
                                        }
                                        [ text
                                            (String.fromInt spread.year
                                                ++ "-"
                                                ++ String.fromInt spread.month
                                                ++ "-"
                                                ++ String.fromInt spread.dayOfMonth
                                            )
                                        ]
                                )
                                model.dailySpreads
                            , List.map
                                (\spread ->
                                    selectOption
                                        { selectOptionConfig
                                            | value = ObjectId.toString spread.objectId
                                        }
                                        [ text
                                            (String.fromInt spread.year
                                                ++ "-"
                                                ++ String.fromInt spread.month
                                            )
                                        ]
                                )
                                model.monthlySpreads
                            , List.map
                                (\spread ->
                                    selectOption
                                        { selectOptionConfig
                                            | value = ObjectId.toString spread.objectId
                                        }
                                        [ text spread.title ]
                                )
                                model.collectionSpreads
                            ]
                        )
                    ]
                ]

        bulletDate =
            Html.div
                [ class "edit-bullet__bullet-date" ]
                [ Html.div
                    [ class "edit-bullet__headline" ]
                    [ text "Date" ]
                , Html.div
                    [ class "edit-bullet__date-wrapper" ]
                    [ bulletDayOfMonth
                    , bulletMonth
                    , bulletYear
                    ]
                ]

        bulletDayOfMonth =
            textField
                { textFieldConfig
                    | label = "DD"
                    , value = Just model.dayOfMonth
                    , onInput = Just (lift << DayOfMonthChanged)
                }

        bulletMonth =
            textField
                { textFieldConfig
                    | label = "MM"
                    , value = Just model.month
                    , onInput = Just (lift << MonthChanged)
                }

        bulletYear =
            textField
                { textFieldConfig
                    | label = "YYYY"
                    , value = Just model.year
                    , onInput = Just (lift << YearChanged)
                }

        saveButton =
            raisedButton
                { buttonConfig
                    | onClick = Just (lift SaveClicked)
                }
                "Save"

        cancelButton =
            textButton
                { buttonConfig
                    | onClick = Just (lift CancelClicked)
                }
                "Cancel"
    in
    Html.div
        [ class "edit-bullet__form-wrapper"
        ]
        [ bulletTipe
        , bulletText
        , bulletDate
        , bulletSpread
        , formButtons
        ]


bulletDelete lift model =
    if model.bulletId /= Nothing then
        deleteBullet lift model

    else
        text ""


deleteBullet lift model =
    Html.div
        [ class "edit-bullet__wrapper"
        ]
        [ Html.h2 [ class "edit-bullet__headline" ] [ text "Delete" ]
        , Html.div [ class "edit-bullet__delete-wrapper" ]
            [ raisedButton
                { buttonConfig
                    | onClick = Just (lift DeleteClicked)
                }
                "Delete"
            ]
        , dialog
            { dialogConfig
                | open = model.showConfirmDeleteDialog
                , onClose = Just (lift ConfirmDeleteDialogClosed)
            }
            { title = Just "Confirm to delete"
            , content =
                [ text """
    Do you really want to delete this bullet?"
                              """
                ]
            , actions =
                [ cancelButton
                    { buttonConfig
                        | onClick = Just (lift CancelDeleteClicked)
                    }
                    "No"
                , acceptButton
                    { buttonConfig
                        | onClick = Just (lift ConfirmDeleteClicked)
                    }
                    "Yes"
                ]
            }
        ]
