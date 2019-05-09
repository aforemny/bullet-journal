module View.EditBullet exposing (Model, Msg(..), Tipe(..), bulletDelete, bulletForm, defaultModel, deleteBullet, init, subscriptions, update, view)

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes as Html
import Material
import Material.Button as Button
import Material.Checkbox as Checkbox
import Material.Dialog as Dialog
import Material.Icon as Icon
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Select as Select
import Material.TextField as TextField
import Material.Toolbar as Toolbar
import Parse
import Parse.Private.ObjectId as ObjectId
import Parse.Private.Pointer as Pointer
import Route exposing (Route)
import Task
import Time
import Type.Bullet as Bullet exposing (Bullet)
import View


type alias Model msg =
    { mdc : Material.Model msg
    , referringUrl : Route
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
    -> Model msg
defaultModel referringUrl bulletId =
    { mdc = Material.defaultModel
    , referringUrl = referringUrl
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
    = Mdc (Material.Msg msg)
    | BulletResult (Result Parse.Error (Parse.Object Bullet))
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
    -> View.Config msg
    -> Route
    -> Maybe (Parse.ObjectId Bullet)
    -> Maybe (Model msg)
    -> ( Model msg, Cmd msg )
init lift viewConfig referringUrl bulletId model =
    ( defaultModel referringUrl bulletId
    , Cmd.batch
        [ Material.init (lift << Mdc)
        , bulletId
            |> Maybe.map (Bullet.get viewConfig.parse)
            |> Maybe.map (Task.attempt (lift << BulletResult))
            |> Maybe.withDefault Cmd.none
        ]
    )


subscriptions : (Msg msg -> msg) -> Model msg -> Sub msg
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


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    Html.div
        [ Html.class "edit-bullet"
        , case model.tipe of
            Task ->
                Html.class "edit-bullet--tipe-task"

            Event ->
                Html.class "edit-bullet--tipe-event"

            Note ->
                Html.class "edit-bullet--tipe-note"
        ]
        [ Icon.view
            [ cs "edit-bullet__back-icon"
            , Options.onClick (lift BackClicked)
            ]
            "arrow_back"
        , Html.div
            [ Html.class "edit-bullet__wrapper"
            ]
            [ bulletForm lift model
            , bulletDelete lift model
            ]
        ]



--          viewConfig.toolbar
--            { title =
--                if model.bulletId /= Nothing then
--                    "Edit bullet"
--                else
--                    "New bullet"
--            , menuIcon =
--                Icon.view
--                    [ Toolbar.menuIcon
--                    , Options.onClick (lift BackClicked)
--                    ]
--                    "arrow_back"
--            , additionalSections = []
--            }


bulletForm lift model =
    let
        formButtons =
            Html.div
                [ Html.class "edit-bullet__buttons-wrapper" ]
                [ saveButton

                -- , cancelButton
                ]

        bulletTipe =
            Html.div
                [ Html.class "edit-bullet__tipe-wrapper" ]
                [ Select.view (lift << Mdc)
                    "edit-bullet__tipe"
                    model.mdc
                    [ Select.label "Type"
                    , Select.preselected
                    , Options.onChange
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
                    ]
                    [ Select.option
                        [ Select.value "task"
                        , when (model.tipe == Task) Select.selected
                        ]
                        [ text "Task"
                        ]
                    , Select.option
                        [ Select.value "event"
                        , when (model.tipe == Event) Select.selected
                        ]
                        [ text "Event"
                        ]
                    , Select.option
                        [ Select.value "note"
                        , when (model.tipe == Note) Select.selected
                        ]
                        [ text "Note"
                        ]
                    ]
                , Html.div
                    [ Html.class "edit-bullet__task-state-wrapper"
                    ]
                    [ Select.view (lift << Mdc)
                        "edit-bullet__task-state"
                        model.mdc
                        [ Select.label "Type"
                        , Select.preselected
                        , Options.onChange
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
                        ]
                        [ Select.option
                            [ Select.value "unchecked"
                            , when (model.taskState == Bullet.Unchecked) Select.selected
                            ]
                            [ text "Unchecked"
                            ]
                        , Select.option
                            [ Select.value "checked"
                            , when (model.taskState == Bullet.Checked) Select.selected
                            ]
                            [ text "Checked"
                            ]
                        , Select.option
                            [ Select.value "migrated"
                            , when (model.taskState == Bullet.Migrated) Select.selected
                            ]
                            [ text "Migrated"
                            ]
                        ]
                    ]
                ]

        bulletText =
            Html.div
                [ Html.class "edit-bullet__text" ]
                [ Html.div
                    [ Html.class "edit-bullet__headline" ]
                    [ text "Text"
                    ]
                , Html.div
                    [ Html.class "edit-bullet__text-wrapper" ]
                    [ TextField.view (lift << Mdc)
                        "edit-bullet__text"
                        model.mdc
                        [ TextField.fullwidth
                        , TextField.value model.text
                        , Options.onInput (lift << TextChanged)
                        ]
                        []
                    ]
                ]

        bulletSpread =
            Html.div
                [ Html.class "edit-bullet__spread" ]
                [ Html.div
                    [ Html.class "edit-bullet__headline" ]
                    [ text "Spread" ]
                , Html.div
                    [ Html.class "edit-bullet__spread-wrapper" ]
                    [ TextField.view (lift << Mdc)
                        "edit-bullet__spread"
                        model.mdc
                        [ TextField.label "Spread"
                        , TextField.value (Maybe.withDefault "" (Maybe.map ObjectId.toString model.spreadId))
                        , TextField.disabled
                        ]
                        []
                    ]
                ]

        bulletDate =
            Html.div
                [ Html.class "edit-bullet__bullet-date" ]
                [ Html.div
                    [ Html.class "edit-bullet__headline" ]
                    [ text "Date" ]
                , Html.div
                    [ Html.class "edit-bullet__date-wrapper" ]
                    [ bulletDayOfMonth
                    , bulletMonth
                    , bulletYear
                    ]
                ]

        bulletDayOfMonth =
            TextField.view (lift << Mdc)
                "edit-bullet__day-of-month"
                model.mdc
                [ TextField.label "DayOfMonth"
                , TextField.value model.dayOfMonth
                , Options.onInput (lift << DayOfMonthChanged)
                ]
                []

        bulletMonth =
            TextField.view (lift << Mdc)
                "edit-bullet__month"
                model.mdc
                [ TextField.label "Month"
                , TextField.value model.month
                , Options.onInput (lift << MonthChanged)
                ]
                []

        bulletYear =
            TextField.view (lift << Mdc)
                "edit-bullet__year"
                model.mdc
                [ TextField.label "Year"
                , TextField.value model.year
                , Options.onInput (lift << YearChanged)
                ]
                []

        saveButton =
            Button.view (lift << Mdc)
                "edit-bullet__save-button"
                model.mdc
                [ Button.ripple
                , Button.raised
                , Button.onClick (lift SaveClicked)
                ]
                [ text "Save" ]

        cancelButton =
            Button.view (lift << Mdc)
                "edit-bullet__cancel-button"
                model.mdc
                [ Button.ripple
                , Button.onClick (lift CancelClicked)
                ]
                [ text "Cancel" ]
    in
    Html.div
        [ Html.class "edit-bullet__form-wrapper"
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
        [ Html.class "edit-bullet__wrapper"
        ]
        [ Html.h2
            [ Html.class "edit-bullet__headline" ]
            [ text "Delete"
            ]
        , Html.div
            [ Html.class "edit-bullet__delete-wrapper"
            ]
            [ Button.view (lift << Mdc)
                "edit-bullet__delete"
                model.mdc
                [ Button.raised
                , Button.ripple
                , Button.onClick (lift DeleteClicked)
                ]
                [ text "Delete"
                ]
            ]
        , Dialog.view (lift << Mdc)
            "edit-bullet__confirm-delete"
            model.mdc
            [ when model.showConfirmDeleteDialog Dialog.open
            , Dialog.onClose (lift ConfirmDeleteDialogClosed)
            ]
            [ styled Html.h3
                [ Dialog.title
                ]
                [ text "Confirm to delete"
                ]
            , Dialog.content []
                [ text """
    Do you really want to delete this bullet?"
                              """
                ]
            , Dialog.actions []
                [ Button.view (lift << Mdc)
                    "edit-bullet__confirm-delete__cancel"
                    model.mdc
                    [ Button.ripple
                    , Dialog.cancel
                    , Button.onClick (lift CancelDeleteClicked)
                    ]
                    [ text "No"
                    ]
                , Button.view (lift << Mdc)
                    "edit-bullet__confirm-delete__accept"
                    model.mdc
                    [ Button.ripple
                    , Dialog.accept
                    , Button.onClick (lift ConfirmDeleteClicked)
                    ]
                    [ text "Yes"
                    ]
                ]
            ]
        ]
