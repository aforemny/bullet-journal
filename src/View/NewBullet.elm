module View.NewBullet exposing (..)

import Date exposing (Date)
import Html.Attributes as Html
import Html exposing (Html, text)
import Material
import Material.Button as Button
import Material.Checkbox as Checkbox
import Material.Options as Options exposing (styled, cs, css, when)
import Material.Select as Select
import Material.Textfield as TextField
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
    , spread : Parse.Pointer Bullet.Any
    , tipe : Tipe
    , taskState : Bullet.TaskState
    , text : String
    , error : Maybe Parse.Error
    }


type Tipe
    = Task
    | Event
    | Note


defaultModel : Url -> Parse.Pointer Bullet.Any -> Model msg
defaultModel referringUrl spread =
    { mdc = Material.defaultModel
    , referringUrl = referringUrl
    , spread = spread
    , tipe = Note
    , taskState = Bullet.Unchecked
    , text = ""
    , error = Nothing
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | SpreadChanged (Parse.Pointer Bullet.Any)
    | TipeChanged Tipe
    | TaskStateChanged Bullet.TaskState
    | TextChanged String
    | SaveClicked
    | SaveResult (Result Parse.Error { objectId : Parse.ObjectId Bullet, createdAt : Date })
    | CancelClicked


init :
    (Msg msg -> msg)
    -> View.Config msg
    -> Url
    -> Parse.Pointer Bullet.Any
    -> Maybe (Model msg)
    -> ( Model msg, Cmd msg )
init lift viewConfig referringUrl spread model =
    ( defaultModel referringUrl spread, Material.init (lift << Mdc) )


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
                        { spread = model.spread
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


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    Html.div
        [ Html.class "new-bullet"
        ]
        [ viewConfig.toolbar
            { additionalSections = []
            }
        , Html.div
            [ Html.class "new-bullet__title"
            ]
            [ text "New bullet"
            ]
        , Html.div
            [ Html.class "new-bullet__wrapper"
            ]
            [ Html.div []
                [ TextField.view (lift << Mdc)
                    "new-bullet__spread"
                    model.mdc
                    [ TextField.label "Spread"
                    , TextField.value (ObjectId.toString (Pointer.objectId model.spread))
                    , TextField.disabled
                    ]
                    []
                ]
            , Html.div []
                [ Select.view (lift << Mdc)
                    "new-bullet__tipe"
                    model.mdc
                    [ Select.label "Spread"
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
                , Checkbox.view (lift << Mdc)
                    "new-bullet__task-state"
                    model.mdc
                    [ if model.taskState /= Bullet.Migrated then
                        Checkbox.checked (model.taskState == Bullet.Checked)
                      else
                        Options.nop
                    , Options.onClick
                        (lift <|
                            TaskStateChanged <|
                                case model.taskState of
                                    Bullet.Checked ->
                                        Bullet.Unchecked

                                    Bullet.Unchecked ->
                                        Bullet.Checked

                                    Bullet.Migrated ->
                                        Bullet.Migrated
                        )
                    , when (model.tipe /= Task) Checkbox.disabled
                    ]
                    []
                , TextField.view (lift << Mdc)
                    "new-bullet__text"
                    model.mdc
                    [ TextField.label "Text"
                    , TextField.value model.text
                    , Options.onInput (lift << TextChanged)
                    ]
                    []
                ]
            , Html.div []
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
        ]
