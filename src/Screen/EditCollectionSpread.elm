module Screen.EditCollectionSpread exposing
    ( Config
    , Model
    , Msg
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
import Material.Dialog exposing (dialog, dialogConfig)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.TextField exposing (textField, textFieldConfig)
import Material.TopAppBar as TopAppBar
import Parse
import Parse.Private.ObjectId as ObjectId
import Route
import Task exposing (Task)
import Time
import Time.Calendar.Days as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)


type alias Config msg =
    { key : Browser.Navigation.Key
    , today : Calendar.Day
    , parse : Parse.Config
    , topAppBar :
        { title : String
        , menuIcon : Maybe (Html msg)
        , additionalSections : List (Html msg)
        }
        -> Html msg
    , fixedAdjust : Html.Attribute msg
    }


type alias Model =
    { collectionSpreadId : Maybe (Parse.ObjectId CollectionSpread)
    , collectionSpread : Maybe (Parse.Object CollectionSpread)
    , error : Maybe Parse.Error
    , showConfirmDeleteDialog : Bool
    , title : String
    }


defaultModel : Maybe (Parse.ObjectId CollectionSpread) -> Model
defaultModel collectionSpreadId =
    { collectionSpreadId = collectionSpreadId
    , collectionSpread = Nothing
    , error = Nothing
    , showConfirmDeleteDialog = False
    , title = ""
    }


type Msg msg
    = CollectionSpreadResult (Result Parse.Error (Parse.Object CollectionSpread))
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
    -> Config msg
    -> Maybe (Parse.ObjectId CollectionSpread)
    -> ( Model, Cmd msg )
init lift viewConfig maybeCollectionSpreadId =
    ( defaultModel maybeCollectionSpreadId
    , maybeCollectionSpreadId
        |> Maybe.map
            (\collectionSpreadId ->
                Task.attempt (lift << CollectionSpreadResult)
                    (CollectionSpread.get viewConfig.parse collectionSpreadId)
            )
        |> Maybe.withDefault Cmd.none
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
update lift viewConfig msg model =
    case msg of
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
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString Route.TableOfContent)
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
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString Route.TableOfContent)
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


view : (Msg msg -> msg) -> Config msg -> Model -> List (Html msg)
view lift viewConfig model =
    let
        collectionSpread_ =
            model.collectionSpread

        collectionSpread =
            collectionSpread_
                |> Maybe.map
                    CollectionSpread.fromParseObject
    in
    [ viewConfig.topAppBar
        { title =
            collectionSpread
                |> Maybe.map CollectionSpread.title
                |> Maybe.withDefault ""
        , menuIcon =
            Just <|
                iconButton
                    { iconButtonConfig
                        | onClick = Just (lift BackClicked)
                        , additionalAttributes = [ TopAppBar.navigationIcon ]
                    }
                    "arrow_back"
        , additionalSections =
            []
        }
    , Html.div
        [ class "edit-collection-spread"
        , viewConfig.fixedAdjust
        ]
        [ Html.div
            [ class "edit-collection-spread__wrapper"
            ]
            [ Html.div
                [ class "edit-collection-spread__title-wrapper"
                ]
                [ textField
                    { textFieldConfig
                        | label = Just "Title"
                        , value = model.title
                        , onInput = Just (lift << TitleChanged)
                    }
                ]
            , Html.div
                [ class "edit-collection-spread__buttons-wrapper"
                ]
                [ raisedButton
                    { buttonConfig
                        | onClick = Just (lift SaveClicked)
                    }
                    "Save"
                , raisedButton
                    { buttonConfig
                        | onClick = Just (lift CancelClicked)
                    }
                    "Cancel"
                ]
            ]
        , if model.collectionSpread /= Nothing then
            Html.div
                [ class "edit-collection-spread__wrapper"
                ]
                [ Html.h2
                    [ class "edit-collection-spread__headline" ]
                    [ text "Delete"
                    ]
                , Html.div
                    [ class "edit-collection-spread__delete-wrapper"
                    ]
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
Do you really want to delete this collection spread?"
                          """
                        ]
                    , actions =
                        [ textButton
                            { buttonConfig
                                | onClick = Just (lift CancelDeleteClicked)
                            }
                            "No"
                        , textButton
                            { buttonConfig
                                | onClick = Just (lift ConfirmDeleteClicked)
                            }
                            "Yes"
                        ]
                    }
                ]

          else
            text ""
        ]
    ]
