module Screen.EditBullet exposing
    ( Model
    , Msg(..)
    , defaultModel
    , init
    , subscriptions
    , update
    , view
    )

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
import Task
import Time
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar
import Time.Format.Locale as Calendar
import Type.Bullet as Bullet exposing (Bullet)


type alias Model =
    { referringUrl : Maybe Route
    , bulletId : Maybe (Parse.ObjectId Bullet)
    , bullet : Maybe (Parse.Object Bullet)
    , tipe : Tipe
    , taskState : Bullet.TaskState
    , text : String
    , year : String
    , month : String
    , dayOfMonth : String
    , error : Maybe Parse.Error
    , showConfirmDeleteDialog : Bool
    }


type alias Config msg =
    { key : Browser.Navigation.Key
    , parse : Parse.Config
    , fixedAdjust : Html.Attribute msg
    , topAppBar :
        { title : String
        , menuIcon : Maybe (Html msg)
        , additionalSections : List (Html msg)
        }
        -> Html msg
    }


type Tipe
    = Task
    | Event
    | Note


defaultModel :
    Maybe Route
    -> Maybe (Parse.ObjectId Bullet)
    -> Model
defaultModel referringUrl bulletId =
    { referringUrl = referringUrl
    , bulletId = bulletId
    , bullet = Nothing
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
    -> Config msg
    -> Maybe Route
    -> Maybe (Parse.ObjectId Bullet)
    -> ( Model, Cmd msg )
init lift config referringUrl bulletId =
    ( defaultModel referringUrl bulletId
    , Cmd.batch
        [ bulletId
            |> Maybe.map (Bullet.get config.parse)
            |> Maybe.map (Task.attempt (lift << BulletResult))
            |> Maybe.withDefault Cmd.none
        ]
    )


subscriptions : (Msg msg -> msg) -> Model -> Sub msg
subscriptions lift model =
    Sub.none


update :
    (Msg msg -> msg)
    -> Config msg
    -> Msg msg
    -> Model
    -> ( Model, Cmd msg )
update lift config msg model =
    let
        referringUrl =
            Maybe.withDefault Route.Overview model.referringUrl
    in
    case msg of
        DayOfMonthChanged dayOfMonth ->
            ( { model | dayOfMonth = dayOfMonth }, Cmd.none )

        MonthChanged month ->
            ( { model | month = month }, Cmd.none )

        YearChanged year ->
            ( { model | year = year }, Cmd.none )

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
                ctor =
                    case model.tipe of
                        Task ->
                            Bullet.Task

                        Event ->
                            Bullet.Event

                        Note ->
                            Bullet.Note

                maybeBullet =
                    Maybe.map2
                        (\year month ->
                            { ctor = ctor
                            , text = model.text
                            , date =
                                case String.toInt model.dayOfMonth of
                                    Just dayOfMonth ->
                                        Bullet.DayDate
                                            { year = year
                                            , month = month
                                            , dayOfMonth = dayOfMonth
                                            }

                                    Nothing ->
                                        Bullet.MonthDate { year = year, month = month }
                            , taskState =
                                if ctor == Bullet.Task then
                                    Just model.taskState

                                else
                                    Nothing
                            }
                        )
                        (String.toInt model.year)
                        (String.toInt model.month)
            in
            ( model
            , maybeBullet
                |> Maybe.map
                    (\bullet ->
                        model.bulletId
                            |> Maybe.map
                                (\bulletId ->
                                    Task.attempt (lift << UpdateResult)
                                        (Bullet.update config.parse bulletId bullet)
                                )
                            |> Maybe.withDefault
                                (Task.attempt (lift << CreateResult)
                                    (Bullet.create config.parse bullet)
                                )
                    )
                |> Maybe.withDefault Cmd.none
            )

        UpdateResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        UpdateResult (Ok _) ->
            ( model
            , Browser.Navigation.pushUrl config.key (Route.toString referringUrl)
            )

        CreateResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        CreateResult (Ok _) ->
            ( model
            , Browser.Navigation.pushUrl config.key (Route.toString referringUrl)
            )

        CancelClicked ->
            ( model
            , Browser.Navigation.pushUrl config.key (Route.toString referringUrl)
            )

        BackClicked ->
            ( model
            , Browser.Navigation.pushUrl config.key (Route.toString referringUrl)
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
                |> Maybe.map (Bullet.delete config.parse)
                |> Maybe.map (Task.attempt (lift << DeleteResult))
                |> Maybe.withDefault Cmd.none
            )

        DeleteResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DeleteResult (Ok _) ->
            ( model
            , Browser.Navigation.pushUrl config.key (Route.toString referringUrl)
            )

        BulletResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        BulletResult (Ok bullet) ->
            ( { model
                | bullet = Just bullet
                , tipe =
                    case bullet.ctor of
                        Bullet.Event ->
                            Event

                        Bullet.Note ->
                            Note

                        Bullet.Task ->
                            Task
                , taskState = Maybe.withDefault Bullet.Unchecked bullet.taskState
                , text = bullet.text
                , year =
                    case bullet.date of
                        Bullet.DayDate { year } ->
                            String.fromInt year

                        Bullet.MonthDate { year } ->
                            String.fromInt year
                , month =
                    case bullet.date of
                        Bullet.DayDate { month } ->
                            String.fromInt month

                        Bullet.MonthDate { month } ->
                            String.fromInt month
                , dayOfMonth =
                    case bullet.date of
                        Bullet.DayDate { dayOfMonth } ->
                            String.fromInt dayOfMonth

                        Bullet.MonthDate _ ->
                            ""
              }
            , Cmd.none
            )


view : (Msg msg -> msg) -> Config msg -> Model -> List (Html msg)
view lift config model =
    [ config.topAppBar
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
        , config.fixedAdjust
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


bulletForm : (Msg msg -> msg) -> Model -> Html msg
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
            filledSelect
                { selectConfig
                    | label = "DD"
                    , value = Just model.dayOfMonth
                    , onChange = Just (lift << DayOfMonthChanged)
                }
                ((::)
                    (selectOption { selectOptionConfig | value = "" } [ text "" ])
                    (model.bullet
                        |> Maybe.map
                            (\bullet ->
                                let
                                    { year, month } =
                                        case bullet.date of
                                            Bullet.MonthDate date ->
                                                { year = date.year
                                                , month = date.month
                                                }

                                            Bullet.DayDate date ->
                                                { year = date.year
                                                , month = date.month
                                                }

                                    lastDayOfMonth =
                                        Calendar.monthLength (Calendar.isLeapYear year)
                                            month
                                in
                                List.range 1 lastDayOfMonth
                                    |> List.map
                                        (\dayOfMonth ->
                                            let
                                                day =
                                                    Calendar.fromGregorian year
                                                        month
                                                        dayOfMonth
                                            in
                                            selectOption
                                                { selectOptionConfig
                                                    | value = String.fromInt dayOfMonth
                                                }
                                                [ text <|
                                                    (case Calendar.dayOfWeek day of
                                                        Calendar.Monday ->
                                                            "Mo"

                                                        Calendar.Tuesday ->
                                                            "Tu"

                                                        Calendar.Wednesday ->
                                                            "We"

                                                        Calendar.Thursday ->
                                                            "Th"

                                                        Calendar.Friday ->
                                                            "Fr"

                                                        Calendar.Saturday ->
                                                            "Sa"

                                                        Calendar.Sunday ->
                                                            "Su"
                                                    )
                                                        ++ ", "
                                                        ++ String.fromInt dayOfMonth
                                                        ++ "."
                                                ]
                                        )
                            )
                        |> Maybe.withDefault []
                    )
                )

        bulletMonth =
            filledSelect
                { selectConfig
                    | label = "MM"
                    , value = Just model.month
                    , onChange = Just (lift << MonthChanged)
                }
                (List.map
                    (\month ->
                        selectOption
                            { selectOptionConfig | value = String.fromInt month }
                            [ text
                                (case Calendar.defaultTimeLocale of
                                    Calendar.TimeLocale { months } ->
                                        List.drop (month - 1) months
                                            |> List.head
                                            |> Maybe.map Tuple.second
                                            |> Maybe.withDefault ""
                                )
                            ]
                    )
                    (List.range 1 12)
                )

        bulletYear =
            filledSelect
                { selectConfig
                    | label = "YYYY"
                    , value = Just model.year
                    , onChange = Just (lift << YearChanged)
                }
                (model.bullet
                    |> Maybe.map
                        (\bullet ->
                            List.map
                                (\yearOffset ->
                                    let
                                        value =
                                            String.fromInt <|
                                                (case bullet.date of
                                                    Bullet.MonthDate { year } ->
                                                        year

                                                    Bullet.DayDate { year } ->
                                                        year
                                                )
                                                    + 50
                                                    - yearOffset
                                    in
                                    selectOption
                                        { selectOptionConfig | value = value }
                                        [ text value ]
                                )
                                (List.range 0 100)
                        )
                    |> Maybe.withDefault []
                )

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
