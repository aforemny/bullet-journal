module View.EditMonthlySpread exposing (Model, Msg(..), defaultModel, init, subscriptions, update, view)

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
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
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
    | SaveResult (Result Parse.Error { updatedAt : Time.Posix })
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
                , year = String.fromInt monthlySpread.year
                , month = String.fromInt monthlySpread.month
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
            , model.monthlySpread
                |> Maybe.map (MonthlySpread.delete viewConfig.parse << .objectId)
                |> Maybe.map (Task.attempt (lift << DeleteResult))
                |> Maybe.withDefault Cmd.none
            )

        DeleteResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DeleteResult (Ok _) ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString Route.Index)
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
                    [ styled Html.h3
                        [ Dialog.title
                        ]
                        [ text "Confirm to delete"
                        ]
                    , Dialog.content []
                        [ text """
Do you really want to delete this monthly spread?
                          """
                        ]
                    , Dialog.actions []
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
                ]

          else
            text ""
        ]
