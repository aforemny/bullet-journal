module Screen.EditDailySpread exposing (Model, Msg(..), defaultModel, init, subscriptions, update, view)

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Material.Button exposing (buttonConfig, raisedButton)
import Material.Dialog exposing (acceptButton, cancelButton, dialog, dialogConfig)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.List exposing (list, listConfig, listItem, listItemConfig)
import Material.TextField exposing (textField, textFieldConfig)
import Material.TopAppBar as TopAppBar
import Parse
import Parse.Private.ObjectId as ObjectId
import Route
import Screen
import Task exposing (Task)
import Time
import Type.Bullet as Bullet exposing (Bullet)
import Type.DailySpread as DailySpread exposing (DailySpread)


type alias Model =
    { dailySpread : Maybe (Parse.Object DailySpread)
    , error : Maybe Parse.Error
    , showConfirmDeleteDialog : Bool
    , year : String
    , month : String
    , dayOfMonth : String
    }


defaultModel : Model
defaultModel =
    { dailySpread = Nothing
    , error = Nothing
    , showConfirmDeleteDialog = False
    , year = ""
    , month = ""
    , dayOfMonth = ""
    }


type Msg msg
    = DailySpreadResult (Result Parse.Error (Parse.Object DailySpread))
    | BackClicked
    | DeleteClicked
    | ConfirmDeleteDialogClosed
    | CancelDeleteClicked
    | ConfirmDeleteClicked
    | DeleteResult (Result Parse.Error {})
    | MonthChanged String
    | YearChanged String
    | DayOfMonthChanged String
    | SaveClicked
    | SaveResult (Result Parse.Error { updatedAt : Time.Posix })
    | CancelClicked


init :
    (Msg msg -> msg)
    -> Screen.Config msg
    -> Parse.ObjectId DailySpread
    -> Model
    -> ( Model, Cmd msg )
init lift viewConfig objectId model =
    ( defaultModel
    , Cmd.batch
        [ Task.attempt (lift << DailySpreadResult)
            (DailySpread.get viewConfig.parse objectId)
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
        DailySpreadResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DailySpreadResult (Ok dailySpread) ->
            ( { model
                | dailySpread = Just dailySpread
                , year = String.fromInt dailySpread.year
                , month = String.fromInt dailySpread.month
                , dayOfMonth = String.fromInt dailySpread.dayOfMonth
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
            , model.dailySpread
                |> Maybe.map (DailySpread.delete viewConfig.parse << .objectId)
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

        DayOfMonthChanged dayOfMonth ->
            ( { model | dayOfMonth = dayOfMonth }, Cmd.none )

        MonthChanged month ->
            ( { model | month = month }, Cmd.none )

        SaveClicked ->
            ( model
            , model.dailySpread
                |> Maybe.map
                    (\dailySpread ->
                        { dailySpread
                            | year =
                                String.toInt model.year
                                    |> Maybe.withDefault dailySpread.year
                            , month =
                                String.toInt model.month
                                    |> Maybe.withDefault dailySpread.month
                            , dayOfMonth =
                                String.toInt model.dayOfMonth
                                    |> Maybe.withDefault dailySpread.dayOfMonth
                        }
                    )
                |> Maybe.map
                    (\dailySpread ->
                        DailySpread.update viewConfig.parse
                            dailySpread.objectId
                            (DailySpread.fromParseObject dailySpread)
                    )
                |> Maybe.map (Task.attempt (lift << SaveResult))
                |> Maybe.withDefault Cmd.none
            )

        SaveResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        SaveResult (Ok _) ->
            ( model
            , model.dailySpread
                |> Maybe.map (Route.DailySpread << .objectId)
                |> Maybe.map (Browser.Navigation.pushUrl viewConfig.key << Route.toString)
                |> Maybe.withDefault Cmd.none
            )

        CancelClicked ->
            ( model
            , model.dailySpread
                |> Maybe.map (Route.DailySpread << .objectId)
                |> Maybe.map (Browser.Navigation.pushUrl viewConfig.key << Route.toString)
                |> Maybe.withDefault Cmd.none
            )


view : (Msg msg -> msg) -> Screen.Config msg -> Model -> List (Html msg)
view lift viewConfig model =
    let
        dailySpread_ =
            model.dailySpread

        dailySpread =
            dailySpread_
                |> Maybe.map
                    DailySpread.fromParseObject
    in
    [ viewConfig.topAppBar
        { title =
            dailySpread
                |> Maybe.map DailySpread.title
                |> Maybe.withDefault ""
        , menuIcon =
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
        [ class "edit-daily-spread"
        , viewConfig.fixedAdjust
        ]
        [ Html.div
            [ class "edit-daily-spread__wrapper"
            ]
            [ Html.div
                [ class "edit-daily-spread__year-wrapper"
                ]
                [ textField
                    { textFieldConfig
                        | label = "Year"
                        , value = Just model.year
                        , onInput = Just (lift << YearChanged)
                    }
                ]
            , Html.div [ class "edit-daily-spread__month-wrapper" ]
                [ textField
                    { textFieldConfig
                        | label = "Month"
                        , value = Just model.month
                        , onInput = Just (lift << MonthChanged)
                    }
                ]
            , Html.div [ class "edit-daily-spread__day-of-month-wrapper" ]
                [ textField
                    { textFieldConfig
                        | label = "Day of month"
                        , value = Just model.dayOfMonth
                        , onInput = Just (lift << DayOfMonthChanged)
                    }
                ]
            , Html.div
                [ class "edit-daily-spread__buttons-wrapper"
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
        , if model.dailySpread /= Nothing then
            Html.div
                [ class "edit-daily-spread__wrapper"
                ]
                [ Html.h2
                    [ class "edit-daily-spread__headline" ]
                    [ text "Delete"
                    ]
                , Html.div
                    [ class "edit-daily-spread__delete-wrapper"
                    ]
                    [ raisedButton
                        { buttonConfig
                            | onClick = Just (lift DeleteClicked)
                        }
                        "Delete"
                    ]
                , dialog
                    { dialogConfig
                        | onClose = Just (lift ConfirmDeleteDialogClosed)
                        , open = model.showConfirmDeleteDialog
                    }
                    { title = Just "Confirm to delete"
                    , content =
                        [ text "Do you really want to delete this daily spread?"
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
