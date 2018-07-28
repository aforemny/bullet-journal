module View.EditMonthlySpread exposing (..)

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
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import Url
import View


type alias Model msg =
    { mdc : Material.Model msg
    , monthlySpread : Maybe (Parse.Object MonthlySpread)
    , error : Maybe Parse.Error
    , showConfirmDeleteDialog : Bool
    , year : String
    , month : String
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    , monthlySpread = Nothing
    , error = Nothing
    , showConfirmDeleteDialog = False
    , year = ""
    , month = ""
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | MonthlySpreadResult (Result Parse.Error (Parse.Object MonthlySpread))
    | BackClicked
    | DeleteClicked
    | ConfirmDeleteDialogClosed
    | CancelDeleteClicked
    | ConfirmDeleteClicked
    | DeleteResult (Result Parse.Error {})
    | MonthChanged String
    | YearChanged String
    | SaveClicked
    | SaveResult (Result Parse.Error { updatedAt : Date })
    | CancelClicked


init :
    (Msg msg -> msg)
    -> View.Config msg
    -> Parse.ObjectId MonthlySpread
    -> Model msg
    -> ( Model msg, Cmd msg )
init lift viewConfig objectId model =
    ( defaultModel
    , Cmd.batch
        [ Material.init (lift << Mdc)
        , Task.attempt (lift << MonthlySpreadResult)
            (MonthlySpread.get viewConfig.parse objectId)
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

        MonthlySpreadResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        MonthlySpreadResult (Ok monthlySpread) ->
            ( { model
                | monthlySpread = Just monthlySpread
                , year = toString monthlySpread.year
                , month = toString monthlySpread.month
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
            , model.monthlySpread
                |> Maybe.map (MonthlySpread.delete viewConfig.parse << .objectId)
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
                                  |> Result.toMaybe
                                  |> Maybe.withDefault monthlySpread.year
                            , month =
                                  String.toInt model.month
                                  |> Result.toMaybe
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
                |> Maybe.map (Url.MonthlySpread << .objectId)
                |> Maybe.map (Navigation.newUrl << Url.toString)
                |> Maybe.withDefault Cmd.none
            )

        CancelClicked ->
            ( model
            , model.monthlySpread
                |> Maybe.map (Url.MonthlySpread << .objectId)
                |> Maybe.map (Navigation.newUrl << Url.toString)
                |> Maybe.withDefault Cmd.none
            )


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    let
        monthlySpread_ =
            model.monthlySpread

        monthlySpread =
            monthlySpread_
                |> Maybe.map
                    MonthlySpread.fromParseObject
    in
        Html.div
            [ Html.class "edit-monthly-spread"
            ]
            [ viewConfig.toolbar
                { title =
                    monthlySpread
                        |> Maybe.map MonthlySpread.title
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
                [ Html.class "edit-monthly-spread__wrapper"
                ]
                [ Html.div
                    [ Html.class "edit-monthly-spread__year-wrapper"
                    ]
                    [ TextField.view (lift << Mdc)
                        "edit-monthly-spread__year"
                        model.mdc
                        [ TextField.label "Year"
                        , TextField.value model.year
                        , Options.onInput (lift << YearChanged)
                        ]
                        []
                    ]
                , Html.div
                    [ Html.class "edit-monthly-spread__month-wrapper"
                    ]
                    [ TextField.view (lift << Mdc)
                        "edit-monthly-spread__month"
                        model.mdc
                        [ TextField.label "Month"
                        , TextField.value model.month
                        , Options.onInput (lift << MonthChanged)
                        ]
                        []
                    ]
                , Html.div
                    [ Html.class "edit-monthly-spread__buttons-wrapper"
                    ]
                    [ Button.view (lift << Mdc)
                        "edit-monthly-spread__save-button"
                        model.mdc
                        [ Button.ripple
                        , Button.raised
                        , Button.onClick (lift SaveClicked)
                        ]
                        [ text "Save" ]
                    , Button.view (lift << Mdc)
                        "edit-monthly-spread__cancel-button"
                        model.mdc
                        [ Button.ripple
                        , Button.onClick (lift CancelClicked)
                        ]
                        [ text "Cancel" ]
                    ]
                ]
            , if model.monthlySpread /= Nothing then
                Html.div
                    [ Html.class "edit-monthly-spread__wrapper"
                    ]
                    [ Html.h2
                        [ Html.class "edit-monthly-spread__headline" ]
                        [ text "Delete"
                        ]
                    , Html.div
                        [ Html.class "edit-monthly-spread__delete-wrapper"
                        ]
                        [ Button.view (lift << Mdc)
                            "edit-monthly-spread__delete"
                            model.mdc
                            [ Button.raised
                            , Button.ripple
                            , Button.onClick (lift DeleteClicked)
                            ]
                            [ text "Delete"
                            ]
                        ]
                    , Dialog.view (lift << Mdc)
                        "edit-monthly-spread__confirm-delete"
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
                                    "edit-monthly-spread__confirm-delete__cancel"
                                    model.mdc
                                    [ Button.ripple
                                    , Dialog.cancel
                                    , Button.onClick (lift CancelDeleteClicked)
                                    ]
                                    [ text "No"
                                    ]
                                , Button.view (lift << Mdc)
                                    "edit-monthly-spread__confirm-delete__accept"
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
