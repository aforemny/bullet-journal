module View.EditCollectionSpread exposing (Model, Msg(..), defaultModel, init, subscriptions, update, view)

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes as Html
import Html.Events as Html
import Material
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options exposing (cs, css, styled, when)
import Material.TextField as TextField
import Material.Toolbar as Toolbar
import Parse
import Parse.Private.ObjectId as ObjectId
import Route
import Task exposing (Task)
import Time
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
import View


type alias Model msg =
    { mdc : Material.Model msg
    , collectionSpread : Maybe (Parse.Object CollectionSpread)
    , error : Maybe Parse.Error
    , showConfirmDeleteDialog : Bool
    , title : String
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    , collectionSpread = Nothing
    , error = Nothing
    , showConfirmDeleteDialog = False
    , title = ""
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | CollectionSpreadResult (Result Parse.Error (Parse.Object CollectionSpread))
    | BackClicked
    | DeleteClicked
    | ConfirmDeleteDialogClosed
    | CancelDeleteClicked
    | ConfirmDeleteClicked
    | DeleteResult (Result Parse.Error {})
    | TitleChanged String
    | SaveClicked
    | SaveResult (Result Parse.Error { updatedAt : Time.Posix })
    | CancelClicked


init :
    (Msg msg -> msg)
    -> View.Config msg
    -> Parse.ObjectId CollectionSpread
    -> Model msg
    -> ( Model msg, Cmd msg )
init lift viewConfig objectId model =
    ( defaultModel
    , Cmd.batch
        [ Material.init (lift << Mdc)
        , Task.attempt (lift << CollectionSpreadResult)
            (CollectionSpread.get viewConfig.parse objectId)
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

        CollectionSpreadResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        CollectionSpreadResult (Ok collectionSpread) ->
            ( { model
                | collectionSpread = Just collectionSpread
                , title = collectionSpread.title
              }
            , Cmd.none
            )

        BackClicked ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString Route.Index)
            )

        DeleteClicked ->
            ( { model | showConfirmDeleteDialog = True }, Cmd.none )

        ConfirmDeleteDialogClosed ->
            ( { model | showConfirmDeleteDialog = False }, Cmd.none )

        CancelDeleteClicked ->
            ( { model | showConfirmDeleteDialog = False }, Cmd.none )

        ConfirmDeleteClicked ->
            ( model
            , model.collectionSpread
                |> Maybe.map (CollectionSpread.delete viewConfig.parse << .objectId)
                |> Maybe.map (Task.attempt (lift << DeleteResult))
                |> Maybe.withDefault Cmd.none
            )

        DeleteResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DeleteResult (Ok _) ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString Route.Index)
            )

        TitleChanged title ->
            ( { model | title = title }, Cmd.none )

        SaveClicked ->
            ( model
            , model.collectionSpread
                |> Maybe.map
                    (\collectionSpread ->
                        { collectionSpread | title = model.title }
                    )
                |> Maybe.map
                    (\collectionSpread ->
                        CollectionSpread.update viewConfig.parse
                            collectionSpread.objectId
                            (CollectionSpread.fromParseObject collectionSpread)
                    )
                |> Maybe.map (Task.attempt (lift << SaveResult))
                |> Maybe.withDefault Cmd.none
            )

        SaveResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        SaveResult (Ok _) ->
            ( model
            , model.collectionSpread
                |> Maybe.map (Route.CollectionSpread << .objectId)
                |> Maybe.map (Browser.Navigation.pushUrl viewConfig.key << Route.toString)
                |> Maybe.withDefault Cmd.none
            )

        CancelClicked ->
            ( model
            , model.collectionSpread
                |> Maybe.map (Route.CollectionSpread << .objectId)
                |> Maybe.map (Browser.Navigation.pushUrl viewConfig.key << Route.toString)
                |> Maybe.withDefault Cmd.none
            )


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    let
        collectionSpread_ =
            model.collectionSpread

        collectionSpread =
            collectionSpread_
                |> Maybe.map
                    CollectionSpread.fromParseObject
    in
    Html.div
        [ Html.class "edit-collection-spread"
        ]
        [ viewConfig.toolbar
            { title =
                collectionSpread
                    |> Maybe.map CollectionSpread.title
                    |> Maybe.withDefault ""
            , menuIcon =
                Icon.view
                    [ Toolbar.menuIcon
                    , Options.onClick (lift BackClicked)
                    ]
                    "arrow_back"
            , additionalSections =
                []
            }
        , Html.div
            [ Html.class "edit-collection-spread__wrapper"
            ]
            [ Html.div
                [ Html.class "edit-collection-spread__title-wrapper"
                ]
                [ TextField.view (lift << Mdc)
                    "edit-collection-spread__title"
                    model.mdc
                    [ TextField.label "Title"
                    , TextField.value model.title
                    , Options.onInput (lift << TitleChanged)
                    ]
                    []
                ]
            , Html.div
                [ Html.class "edit-collection-spread__buttons-wrapper"
                ]
                [ Button.view (lift << Mdc)
                    "edit-collection-spread__save-button"
                    model.mdc
                    [ Button.ripple
                    , Button.raised
                    , Button.onClick (lift SaveClicked)
                    ]
                    [ text "Save" ]
                , Button.view (lift << Mdc)
                    "edit-collection-spread__cancel-button"
                    model.mdc
                    [ Button.ripple
                    , Button.onClick (lift CancelClicked)
                    ]
                    [ text "Cancel" ]
                ]
            ]
        , if model.collectionSpread /= Nothing then
            Html.div
                [ Html.class "edit-collection-spread__wrapper"
                ]
                [ Html.h2
                    [ Html.class "edit-collection-spread__headline" ]
                    [ text "Delete"
                    ]
                , Html.div
                    [ Html.class "edit-collection-spread__delete-wrapper"
                    ]
                    [ Button.view (lift << Mdc)
                        "edit-collection-spread__delete"
                        model.mdc
                        [ Button.raised
                        , Button.ripple
                        , Button.onClick (lift DeleteClicked)
                        ]
                        [ text "Delete"
                        ]
                    ]
                , Dialog.view (lift << Mdc)
                    "edit-collection-spread__confirm-delete"
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
Do you really want to delete this collection spread?"
                          """
                        ]
                    , Dialog.actions []
                        [ Button.view (lift << Mdc)
                            "edit-collection-spread__confirm-delete__cancel"
                            model.mdc
                            [ Button.ripple
                            , Dialog.cancel
                            , Button.onClick (lift CancelDeleteClicked)
                            ]
                            [ text "No"
                            ]
                        , Button.view (lift << Mdc)
                            "edit-collection-spread__confirm-delete__accept"
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

          else
            text ""
        ]
