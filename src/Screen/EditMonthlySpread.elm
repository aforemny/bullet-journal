module Screen.EditMonthlySpread exposing (Model, Msg(..), defaultModel, init, subscriptions, update, view)

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Material.Button exposing (buttonConfig, textButton)
import Material.Dialog exposing (acceptButton, cancelButton, dialog, dialogConfig)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.TextField exposing (textField, textFieldConfig)
import Material.TopAppBar as TopAppBar
import Parse
import Parse.Private.ObjectId as ObjectId
import Route
import Screen
import Task exposing (Task)
import Time
import Type.Bullet as Bullet exposing (Bullet)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)


type alias Model =
    { monthlySpread : Maybe (Parse.Object MonthlySpread)
    , error : Maybe Parse.Error
    , showConfirmDeleteDialog : Bool
    , year : String
    , month : String
    }


defaultModel : Model
defaultModel =
    { monthlySpread = Nothing
    , error = Nothing
    , showConfirmDeleteDialog = False
    , year = ""
    , month = ""
    }


type Msg msg
    = MonthlySpreadResult (Result Parse.Error (Parse.Object MonthlySpread))
    | BackClicked
    | DeleteClicked
    | ConfirmDeleteDialogClosed
    | CancelDeleteClicked
    | ConfirmDeleteClicked
    | DeleteResult (Result Parse.Error {})
    | MonthChanged String
    | YearChanged String
    | SaveClicked
    | SaveResult (Result Parse.Error { updatedAt : Time.Posix })
    | CancelClicked


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
        MonthlySpreadResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        MonthlySpreadResult (Ok monthlySpread) ->
            ( { model
                | monthlySpread = Just monthlySpread
                , year = String.fromInt monthlySpread.year
                , month = String.fromInt monthlySpread.month
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
            , model.monthlySpread
                |> Maybe.map (MonthlySpread.delete viewConfig.parse << .objectId)
                |> Maybe.map (Task.attempt (lift << DeleteResult))
                |> Maybe.withDefault Cmd.none
            )

        DeleteResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DeleteResult (Ok _) ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString Route.TableOfContent)
            )

        YearChanged year ->
            ( { model | year = year }, Cmd.none )

        MonthChanged month ->
            ( { model | month = month }, Cmd.none )

        SaveClicked ->
            ( model
            , model.monthlySpread
                |> Maybe.map
                    (\monthlySpread ->
                        { monthlySpread
                            | year =
                                String.toInt model.year
                                    |> Maybe.withDefault monthlySpread.year
                            , month =
                                String.toInt model.month
                                    |> Maybe.withDefault monthlySpread.month
                        }
                    )
                |> Maybe.map
                    (\monthlySpread ->
                        MonthlySpread.update viewConfig.parse
                            monthlySpread.objectId
                            (MonthlySpread.fromParseObject monthlySpread)
                    )
                |> Maybe.map (Task.attempt (lift << SaveResult))
                |> Maybe.withDefault Cmd.none
            )

        SaveResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        SaveResult (Ok _) ->
            ( model
            , model.monthlySpread
                |> Maybe.map (Route.MonthlySpread << .objectId)
                |> Maybe.map (Browser.Navigation.pushUrl viewConfig.key << Route.toString)
                |> Maybe.withDefault Cmd.none
            )

        CancelClicked ->
            ( model
            , model.monthlySpread
                |> Maybe.map (Route.MonthlySpread << .objectId)
                |> Maybe.map (Browser.Navigation.pushUrl viewConfig.key << Route.toString)
                |> Maybe.withDefault Cmd.none
            )


view : (Msg msg -> msg) -> Screen.Config msg -> Model -> List (Html msg)
view lift viewConfig model =
    let
        monthlySpread_ =
            model.monthlySpread

        monthlySpread =
            monthlySpread_
                |> Maybe.map
                    MonthlySpread.fromParseObject
    in
    [ viewConfig.topAppBar
        { title =
            monthlySpread
                |> Maybe.map MonthlySpread.title
                |> Maybe.withDefault ""
        , menuIcon =
            Just <|
                iconButton
                    { iconButtonConfig
                        | onClick = Just (lift BackClicked)
                        , additionalAttributes = [ TopAppBar.navigationIcon ]
                    }
                    "arrow_back"
        , additionalSections = []
        }
    , Html.div
        [ class "edit-monthly-spread"
        , viewConfig.fixedAdjust
        ]
        [ Html.div
            [ class "edit-monthly-spread__wrapper"
            ]
            [ Html.div
                [ class "edit-monthly-spread__year-wrapper"
                ]
                [ textField
                    { textFieldConfig
                        | label = "Year"
                        , value = Just model.year
                        , onInput = Just (lift << YearChanged)
                    }
                ]
            , Html.div
                [ class "edit-monthly-spread__month-wrapper"
                ]
                [ textField
                    { textFieldConfig
                        | label = "Month"
                        , value = Just model.month
                        , onInput = Just (lift << MonthChanged)
                    }
                ]
            , Html.div
                [ class "edit-monthly-spread__buttons-wrapper"
                ]
                [ textButton
                    { buttonConfig
                        | onClick = Just (lift SaveClicked)
                    }
                    "Save"
                , textButton
                    { buttonConfig
                        | onClick = Just (lift CancelClicked)
                    }
                    "Cancel"
                ]
            ]
        , if model.monthlySpread /= Nothing then
            Html.div
                [ class "edit-monthly-spread__wrapper"
                ]
                [ Html.h2
                    [ class "edit-monthly-spread__headline" ]
                    [ text "Delete"
                    ]
                , Html.div
                    [ class "edit-monthly-spread__delete-wrapper"
                    ]
                    [ textButton { buttonConfig | onClick = Just (lift DeleteClicked) }
                        "Delete"
                    ]
                , dialog
                    { dialogConfig
                        | onClose = Just (lift ConfirmDeleteDialogClosed)
                        , open = model.showConfirmDeleteDialog
                    }
                    { title = Just "Confirm to delete"
                    , content =
                        [ text """
Do you really want to delete this monthly spread?
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

          else
            text ""
        ]
    ]
