module View.EditDailySpread exposing (..)

import Date exposing (Date)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Material
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options exposing (styled, cs, css, when)
import Material.Textfield as TextField
import Material.Toolbar as Toolbar
import Navigation
import Parse
import Private.ObjectId as ObjectId
import Task exposing (Task)
import Type.Bullet as Bullet exposing (Bullet)
import Type.DailySpread as DailySpread exposing (DailySpread)
import Url
import View


type alias Model msg =
    { mdc : Material.Model msg
    , dailySpread : Maybe (Parse.Object DailySpread)
    , error : Maybe Parse.Error
    , showConfirmDeleteDialog : Bool
    , year : String
    , month : String
    , dayOfMonth : String
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    , dailySpread = Nothing
    , error = Nothing
    , showConfirmDeleteDialog = False
    , year = ""
    , month = ""
    , dayOfMonth = ""
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | DailySpreadResult (Result Parse.Error (Parse.Object DailySpread))
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
    | SaveResult (Result Parse.Error { updatedAt : Date })
    | CancelClicked


init :
    (Msg msg -> msg)
    -> View.Config msg
    -> Parse.ObjectId DailySpread
    -> Model msg
    -> ( Model msg, Cmd msg )
init lift viewConfig objectId model =
    ( defaultModel
    , Cmd.batch
        [ Material.init (lift << Mdc)
        , Task.attempt (lift << DailySpreadResult)
            (DailySpread.get viewConfig.parse objectId)
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

        DailySpreadResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DailySpreadResult (Ok dailySpread) ->
            ( { model
                | dailySpread = Just dailySpread
                , year = toString dailySpread.year
                , month = toString dailySpread.month
                , dayOfMonth = toString dailySpread.dayOfMonth
              }
            , Cmd.none
            )

        BackClicked ->
            ( model
            , Navigation.newUrl (Url.toString Url.Index)
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
            , Navigation.newUrl (Url.toString Url.Index)
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
                                  |> Result.toMaybe
                                  |> Maybe.withDefault dailySpread.year
                            , month =
                                  String.toInt model.month
                                  |> Result.toMaybe
                                  |> Maybe.withDefault dailySpread.month
                            , dayOfMonth =
                                  String.toInt model.dayOfMonth
                                  |> Result.toMaybe
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
                |> Maybe.map (Url.DailySpread << .objectId)
                |> Maybe.map (Navigation.newUrl << Url.toString)
                |> Maybe.withDefault Cmd.none
            )

        CancelClicked ->
            ( model
            , model.dailySpread
                |> Maybe.map (Url.DailySpread << .objectId)
                |> Maybe.map (Navigation.newUrl << Url.toString)
                |> Maybe.withDefault Cmd.none
            )


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    let
        dailySpread_ =
            model.dailySpread

        dailySpread =
            dailySpread_
                |> Maybe.map
                    DailySpread.fromParseObject
    in
        Html.div
            [ Html.class "edit-daily-spread"
            ]
            [ viewConfig.toolbar
                { title =
                    dailySpread
                        |> Maybe.map DailySpread.title
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
                [ Html.class "edit-daily-spread__wrapper"
                ]
                [ Html.div
                    [ Html.class "edit-daily-spread__year-wrapper"
                    ]
                    [ TextField.view (lift << Mdc)
                        "edit-daily-spread__year"
                        model.mdc
                        [ TextField.label "Year"
                        , TextField.value model.year
                        , Options.onInput (lift << YearChanged)
                        ]
                        []
                    ]
                , Html.div
                    [ Html.class "edit-daily-spread__month-wrapper"
                    ]
                    [ TextField.view (lift << Mdc)
                        "edit-daily-spread__month"
                        model.mdc
                        [ TextField.label "Month"
                        , TextField.value model.month
                        , Options.onInput (lift << MonthChanged)
                        ]
                        []
                    ]
                , Html.div
                    [ Html.class "edit-daily-spread__day-of-month-wrapper"
                    ]
                    [ TextField.view (lift << Mdc)
                        "edit-daily-spread__day-of-month"
                        model.mdc
                        [ TextField.label "Day of month"
                        , TextField.value model.dayOfMonth
                        , Options.onInput (lift << DayOfMonthChanged)
                        ]
                        []
                    ]
                , Html.div
                    [ Html.class "edit-daily-spread__buttons-wrapper"
                    ]
                    [ Button.view (lift << Mdc)
                        "edit-daily-spread__save-button"
                        model.mdc
                        [ Button.ripple
                        , Button.raised
                        , Button.onClick (lift SaveClicked)
                        ]
                        [ text "Save" ]
                    , Button.view (lift << Mdc)
                        "edit-daily-spread__cancel-button"
                        model.mdc
                        [ Button.ripple
                        , Button.onClick (lift CancelClicked)
                        ]
                        [ text "Cancel" ]
                    ]
                ]
            , if model.dailySpread /= Nothing then
                Html.div
                    [ Html.class "edit-daily-spread__wrapper"
                    ]
                    [ Html.h2
                        [ Html.class "edit-daily-spread__headline" ]
                        [ text "Delete"
                        ]
                    , Html.div
                        [ Html.class "edit-daily-spread__delete-wrapper"
                        ]
                        [ Button.view (lift << Mdc)
                            "edit-daily-spread__delete"
                            model.mdc
                            [ Button.raised
                            , Button.ripple
                            , Button.onClick (lift DeleteClicked)
                            ]
                            [ text "Delete"
                            ]
                        ]
                    , Dialog.view (lift << Mdc)
                        "edit-daily-spread__confirm-delete"
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
Do you really want to delete this daily spread?
                          """
                                ]
                            , Dialog.footer []
                                [ Button.view (lift << Mdc)
                                    "edit-daily-spread__confirm-delete__cancel"
                                    model.mdc
                                    [ Button.ripple
                                    , Dialog.cancel
                                    , Button.onClick (lift CancelDeleteClicked)
                                    ]
                                    [ text "No"
                                    ]
                                , Button.view (lift << Mdc)
                                    "edit-daily-spread__confirm-delete__accept"
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
              else
                text ""
            ]
