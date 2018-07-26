module View.NewBullet exposing (..)

import Date exposing (Date)
import Html.Attributes as Html
import Html exposing (Html, text)
import Material
import Material.Button as Button
import Material.Checkbox as Checkbox
import Material.Dialog as Dialog
import Material.Icon as Icon
import Material.Options as Options exposing (styled, cs, css, when)
import Material.Select as Select
import Material.Textfield as TextField
import Material.Toolbar as Toolbar
import Navigation
import Parse
import Private.ObjectId as ObjectId
import Private.Pointer as Pointer
import Task
import Type.Bullet as Bullet exposing (Bullet)
import Url exposing (Url)
import View


type alias Model msg =
    { mdc : Material.Model msg
    , referringUrl : Url
    , spreadClass : String
    , spread : Parse.ObjectId Bullet.Any
    , tipe : Tipe
    , taskState : Bullet.TaskState
    , text : String
    , error : Maybe Parse.Error
    , showConfirmDeleteDialog : Bool
    }


type Tipe
    = Task
    | Event
    | Note


defaultModel : Url -> String -> Parse.ObjectId Bullet.Any -> Model msg
defaultModel referringUrl spreadClass spread =
    { mdc = Material.defaultModel
    , referringUrl = referringUrl
    , spreadClass = spreadClass
    , spread = spread
    , tipe = Note
    , taskState = Bullet.Unchecked
    , text = ""
    , error = Nothing
    , showConfirmDeleteDialog = True
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | SpreadChanged (Parse.ObjectId Bullet.Any)
    | TipeChanged Tipe
    | TaskStateChanged Bullet.TaskState
    | TextChanged String
    | SaveClicked
    | SaveResult (Result Parse.Error { objectId : Parse.ObjectId Bullet, createdAt : Date })
    | CancelClicked
    | BackClicked
    | DeleteClicked
    | ConfirmDeleteDialogClosed
    | CancelDeleteClicked
    | ConfirmDeleteClicked


init :
    (Msg msg -> msg)
    -> View.Config msg
    -> Url
    -> String
    -> Parse.ObjectId Bullet.Any
    -> Maybe (Model msg)
    -> ( Model msg, Cmd msg )
init lift viewConfig referringUrl spreadClass spread model =
    ( defaultModel referringUrl spreadClass spread, Material.init (lift << Mdc) )


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

        SpreadChanged spread ->
            ( { model | spread = spread }, Cmd.none )

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
                    Just
                        { spreadClass = model.spreadClass
                        , spread = model.spread
                        , state = state
                        , text = model.text
                        }
            in
                ( model
                , bullet
                    |> Maybe.map (Bullet.create viewConfig.parse)
                    |> Maybe.map (Task.attempt (lift << SaveResult))
                    |> Maybe.withDefault Cmd.none
                )

        SaveResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        SaveResult (Ok _) ->
            ( model, Navigation.newUrl (Url.toString model.referringUrl) )

        CancelClicked ->
            ( model, Navigation.newUrl (Url.toString model.referringUrl) )

        BackClicked ->
            ( model, Navigation.newUrl (Url.toString model.referringUrl) )

        DeleteClicked ->
            ( { model | showConfirmDeleteDialog = True }, Cmd.none )

        ConfirmDeleteDialogClosed ->
            ( { model | showConfirmDeleteDialog = False }, Cmd.none )

        CancelDeleteClicked ->
            ( { model | showConfirmDeleteDialog = False }, Cmd.none )

        ConfirmDeleteClicked ->
            ( { model | showConfirmDeleteDialog = False }, Cmd.none )


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    Html.div
        [ Html.class "new-bullet"
        , case model.tipe of
            Task ->
                Html.class "new-bullet--tipe-task"

            Event ->
                Html.class "new-bullet--tipe-event"

            Note ->
                Html.class "new-bullet--tipe-note"
        ]
        [ viewConfig.toolbar
            { title =
                "New bullet"
            , menuIcon =
                Icon.view
                    [ Toolbar.menuIcon
                    , Options.onClick (lift BackClicked)
                    ]
                    "arrow_back"
            , additionalSections = []
            }
        , Html.div
            [ Html.class "new-bullet__wrapper"
            ]
            [ Html.div
                [ Html.class "new-bullet__spread-wrapper"
                ]
                [ TextField.view (lift << Mdc)
                    "new-bullet__spread"
                    model.mdc
                    [ TextField.label "Spread"
                    , TextField.value (ObjectId.toString model.spread)
                    , TextField.disabled
                    ]
                    []
                ]
            , Html.div
                [ Html.class "new-bullet__tipe-wrapper"
                ]
                [ Select.view (lift << Mdc)
                    "new-bullet__tipe"
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
                ]
            , Html.div
                [ Html.class "new-bullet__text-wrapper"
                ]
                [ TextField.view (lift << Mdc)
                    "new-bullet__text"
                    model.mdc
                    [ TextField.label "Text"
                    , TextField.value model.text
                    , Options.onInput (lift << TextChanged)
                    ]
                    []
                ]
            , Html.div
                [ Html.class "new-bullet__task-state-wrapper"
                ]
                [ Select.view (lift << Mdc)
                    "new-bullet__task-state"
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
            , Html.div
                [ Html.class "new-bullet__buttons-wrapper"
                ]
                [ Button.view (lift << Mdc)
                    "new-bullet__save-button"
                    model.mdc
                    [ Button.ripple
                    , Button.raised
                    , Button.onClick (lift SaveClicked)
                    ]
                    [ text "Save" ]
                , Button.view (lift << Mdc)
                    "new-bullet__cancel-button"
                    model.mdc
                    [ Button.ripple
                    , Button.onClick (lift CancelClicked)
                    ]
                    [ text "Cancel" ]
                ]
            ]
        , Html.div
            [ Html.class "new-bullet__wrapper"
            ]
            [ Html.h2
                [ Html.class "new-bullet__headline" ]
                [ text "Delete"
                ]
            , Html.div
                [ Html.class "new-bullet__delete-wrapper"
                ]
                [ Button.view (lift << Mdc)
                    "new-bullet__delete"
                    model.mdc
                    [ Button.raised
                    , Button.ripple
                    , Button.onClick (lift DeleteClicked)
                    ]
                    [ text "Delete"
                    ]
                ]
            , Dialog.view (lift << Mdc)
                "new-bullet__confirm-delete"
                model.mdc
                [ when model.showConfirmDeleteDialog Dialog.open
                , Dialog.onClose (lift ConfirmDeleteDialogClosed)
                ]
                [ Dialog.surface []
                    [ Dialog.header []
                        [ styled Html.h3
                            [ Dialog.title
                            ]
                            [ text "Confirm to delete"
                            ]
                        ]
                    , Dialog.body []
                        [ text """
Do you really want to delete that task?
                          """
                        ]
                    , Dialog.footer []
                        [ Button.view (lift << Mdc)
                            "new-bullet__confirm-delete__cancel"
                            model.mdc
                            [ Button.ripple
                            , Dialog.cancel
                            , Button.onClick (lift CancelDeleteClicked)
                            ]
                            [ text "No"
                            ]
                        , Button.view (lift << Mdc)
                            "new-bullet__confirm-delete__accept"
                            model.mdc
                            [ Button.ripple
                            , Dialog.accept
                            , Button.onClick (lift ConfirmDeleteClicked)
                            ]
                            [ text "Yes"
                            ]
                        ]
                    ]
                , Dialog.backdrop [] []
                ]
            ]
        ]
