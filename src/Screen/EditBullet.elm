module Screen.EditBullet exposing (Model, Msg(..), Tipe(..), bulletDelete, bulletForm, defaultModel, deleteBullet, init, subscriptions, update, view)

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Material.Button exposing (buttonConfig, raisedButton, textButton)
import Material.Checkbox exposing (checkbox, checkboxConfig)
import Material.Dialog exposing (acceptButton, cancelButton, dialog, dialogConfig)
import Material.Icon exposing (icon, iconConfig)
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


init :
    (Msg msg -> msg)
    -> Screen.Config msg
    -> Route
    -> Maybe (Parse.ObjectId Bullet)
    -> Maybe Model
    -> ( Model, Cmd msg )
init lift viewConfig referringUrl bulletId model =
    ( defaultModel referringUrl bulletId
    , bulletId
        |> Maybe.map (Bullet.get viewConfig.parse)
        |> Maybe.map (Task.attempt (lift << BulletResult))
        |> Maybe.withDefault Cmd.none
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


view : (Msg msg -> msg) -> Screen.Config msg -> Model -> List (Html msg)
view lift viewConfig model =
    [ viewConfig.topAppBar
        { title =
            if model.bulletId /= Nothing then
                "Edit bullet"

            else
                "New bullet"
        , menuIcon =
            icon
                { iconConfig
                    | additionalAttributes =
                        [ TopAppBar.navigationIcon
                        , Html.Events.onClick (lift BackClicked)
                        ]
                }
                "arrow_back"
        , additionalSections = []
        }
    , Html.div
        [ class "edit-bullet"
        , viewConfig.fixedAdjust
        , case model.tipe of
            Task ->
                class "edit-bullet--tipe-task"

            Event ->
                class "edit-bullet--tipe-event"

            Note ->
                class "edit-bullet--tipe-note"
        ]
        [ icon
            { iconConfig
                | additionalAttributes =
                    [ class "edit-bullet__back-icon"
                    , Html.Events.onClick (lift BackClicked)
                    ]
            }
            "arrow_back"
        , Html.div [ class "edit-bullet__wrapper" ]
            [ bulletForm lift model
            , bulletDelete lift model
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
                    [ textField
                        { textFieldConfig
                            | label = "Spread"
                            , value = Just (Maybe.withDefault "" (Maybe.map ObjectId.toString model.spreadId))
                            , disabled = True
                        }
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
                    | label = "DayOfMonth"
                    , value = Just model.dayOfMonth
                    , onInput = Just (lift << DayOfMonthChanged)
                }

        bulletMonth =
            textField
                { textFieldConfig
                    | label = "Month"
                    , value = Just model.month
                    , onInput = Just (lift << MonthChanged)
                }

        bulletYear =
            textField
                { textFieldConfig
                    | label = "Year"
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
