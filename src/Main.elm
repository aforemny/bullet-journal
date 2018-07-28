module Main exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Material
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Fab as Fab
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options exposing (styled, cs, css, when)
import Material.Toolbar as Toolbar
import Navigation
import Parse
import Ports
import Private.ObjectId as ObjectId
import Task exposing (Task)
import Time.Calendar.Days as Calendar
import Time.Calendar.Gregorian as Calendar
import Time.Format.Locale as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
import Type.DailySpread as DailySpread exposing (DailySpread)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import Url exposing (Url)
import View
import View.CollectionSpread
import View.DailySpread
import View.EditBullet
import View.EditCollectionSpread
import View.EditDailySpread
import View.EditMonthlySpread
import View.Index
import View.MonthlySpread


type alias Model =
    { mdc : Material.Model Msg
    , url : Url
    , collectionSpread : View.CollectionSpread.Model Msg
    , dailySpread : View.DailySpread.Model Msg
    , index : View.Index.Model Msg
    , monthlySpread : View.MonthlySpread.Model Msg
    , editCollectionSpread : View.EditCollectionSpread.Model Msg
    , editDailySpread : View.EditDailySpread.Model Msg
    , editMonthlySpread : View.EditMonthlySpread.Model Msg
    , editBullet : Maybe (View.EditBullet.Model Msg)
    , today : Calendar.Day
    , now : Date
    , error : Maybe Parse.Error
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , url = Url.Index
    , collectionSpread = View.CollectionSpread.defaultModel
    , dailySpread = View.DailySpread.defaultModel
    , index = View.Index.defaultModel
    , monthlySpread = View.MonthlySpread.defaultModel
    , editMonthlySpread = View.EditMonthlySpread.defaultModel
    , editCollectionSpread = View.EditCollectionSpread.defaultModel
    , editDailySpread = View.EditDailySpread.defaultModel
    , editBullet = Nothing
    , today = Calendar.fromGregorian 1970 1 1
    , now = Date.fromTime 0
    , error = Nothing
    }


type Msg
    = NoOp
    | Mdc (Material.Msg Msg)
    | SetUrl Url
    | TodayChanged Calendar.Day
    | NowChanged Date
    | BackClicked
    | EditBulletMsg (View.EditBullet.Msg Msg)
    | CollectionSpreadMsg (View.CollectionSpread.Msg Msg)
    | DailySpreadMsg (View.DailySpread.Msg Msg)
    | IndexMsg (View.Index.Msg Msg)
    | MonthlySpreadMsg (View.MonthlySpread.Msg Msg)
    | EditCollectionSpreadMsg (View.EditCollectionSpread.Msg Msg)
    | EditDailySpreadMsg (View.EditDailySpread.Msg Msg)
    | EditMonthlySpreadMsg (View.EditMonthlySpread.Msg Msg)
    | TodayClicked
    | TodayClickedResult (Result Parse.Error (Parse.ObjectId DailySpread))
    | MonthClicked
    | MonthClickedResult (Result Parse.Error (Parse.ObjectId MonthlySpread))


type alias Flags =
    { today : String
    , now : String
    }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags (SetUrl << Url.fromLocation)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        url =
            Url.fromLocation location

        viewConfig =
            makeViewConfig model

        model =
            { defaultModel
                | today = Ports.readDayUnsafe flags.today
                , now = Ports.readDateUnsafe flags.now
                , url = url
            }
    in
        ( model
        , Cmd.batch
            [ Material.init Mdc
            ]
        )
            |> initView viewConfig url


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdc model
        , Ports.today TodayChanged
        , Ports.now NowChanged
        , View.CollectionSpread.subscriptions CollectionSpreadMsg model.collectionSpread
        , View.DailySpread.subscriptions DailySpreadMsg model.dailySpread
        , View.Index.subscriptions IndexMsg model.index
        , View.MonthlySpread.subscriptions MonthlySpreadMsg model.monthlySpread
        , View.EditMonthlySpread.subscriptions EditMonthlySpreadMsg model.editMonthlySpread
        , View.EditCollectionSpread.subscriptions EditCollectionSpreadMsg model.editCollectionSpread
        , View.EditDailySpread.subscriptions EditDailySpreadMsg model.editDailySpread
        , model.editBullet
            |> Maybe.map (View.EditBullet.subscriptions EditBulletMsg)
            |> Maybe.withDefault Sub.none
        ]


initView : View.Config Msg -> Url -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initView viewConfig url ( model, cmd ) =
    (case url of
        Url.Index ->
            View.Index.init IndexMsg viewConfig model.index
                |> Tuple.mapFirst
                    (\index ->
                        { model | index = index }
                    )

        Url.MonthlySpread objectId ->
            View.MonthlySpread.init MonthlySpreadMsg
                viewConfig
                objectId
                model.monthlySpread
                |> Tuple.mapFirst
                    (\monthlySpread ->
                        { model | monthlySpread = monthlySpread }
                    )

        Url.DailySpread objectId ->
            View.DailySpread.init
                DailySpreadMsg
                viewConfig
                objectId
                model.dailySpread
                |> Tuple.mapFirst
                    (\dailySpread ->
                        { model | dailySpread = dailySpread }
                    )

        Url.CollectionSpread objectId ->
            View.CollectionSpread.init CollectionSpreadMsg
                viewConfig
                objectId
                model.collectionSpread
                |> Tuple.mapFirst
                    (\collectionSpread ->
                        { model | collectionSpread = collectionSpread }
                    )

        Url.EditMonthlySpread objectId ->
            View.EditMonthlySpread.init EditMonthlySpreadMsg
                viewConfig
                objectId
                model.editMonthlySpread
                |> Tuple.mapFirst
                    (\editMonthlySpread ->
                        { model | editMonthlySpread = editMonthlySpread }
                    )

        Url.EditDailySpread objectId ->
            View.EditDailySpread.init
                EditDailySpreadMsg
                viewConfig
                objectId
                model.editDailySpread
                |> Tuple.mapFirst
                    (\editDailySpread ->
                        { model | editDailySpread = editDailySpread }
                    )

        Url.EditCollectionSpread objectId ->
            View.EditCollectionSpread.init EditCollectionSpreadMsg
                viewConfig
                objectId
                model.editCollectionSpread
                |> Tuple.mapFirst
                    (\editCollectionSpread ->
                        { model | editCollectionSpread = editCollectionSpread }
                    )

        Url.EditBullet route className spreadId bulletId ->
            View.EditBullet.init EditBulletMsg
                viewConfig
                (case className of
                    "CollectionSpread" ->
                        Url.CollectionSpread (Bullet.castObjectId spreadId)

                    "MonthlySpread" ->
                        Url.MonthlySpread (Bullet.castObjectId spreadId)

                    _ ->
                        Url.DailySpread (Bullet.castObjectId spreadId)
                )
                className
                spreadId
                bulletId
                model.editBullet
                |> Tuple.mapFirst
                    (\editBullet ->
                        { model | editBullet = Just editBullet }
                    )

        _ ->
            ( model, Cmd.none )
    )
        |> Tuple.mapSecond
            (\otherCmd ->
                Cmd.batch [ cmd, otherCmd ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        viewConfig =
            makeViewConfig model
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            Mdc msg_ ->
                Material.update Mdc msg_ model

            SetUrl url ->
                ( { model | url = url }, Cmd.none )
                    |> initView viewConfig url

            MonthlySpreadMsg msg_ ->
                model.monthlySpread
                    |> View.MonthlySpread.update MonthlySpreadMsg viewConfig msg_
                    |> Tuple.mapFirst
                        (\monthlySpread -> { model | monthlySpread = monthlySpread })

            DailySpreadMsg msg_ ->
                model.dailySpread
                    |> View.DailySpread.update DailySpreadMsg viewConfig msg_
                    |> Tuple.mapFirst
                        (\dailySpread -> { model | dailySpread = dailySpread })

            CollectionSpreadMsg msg_ ->
                model.collectionSpread
                    |> View.CollectionSpread.update CollectionSpreadMsg viewConfig msg_
                    |> Tuple.mapFirst
                        (\collectionSpread -> { model | collectionSpread = collectionSpread })

            EditMonthlySpreadMsg msg_ ->
                model.editMonthlySpread
                    |> View.EditMonthlySpread.update EditMonthlySpreadMsg viewConfig msg_
                    |> Tuple.mapFirst
                        (\editMonthlySpread -> { model | editMonthlySpread = editMonthlySpread })

            EditDailySpreadMsg msg_ ->
                model.editDailySpread
                    |> View.EditDailySpread.update EditDailySpreadMsg viewConfig msg_
                    |> Tuple.mapFirst
                        (\editDailySpread -> { model | editDailySpread = editDailySpread })

            EditCollectionSpreadMsg msg_ ->
                model.editCollectionSpread
                    |> View.EditCollectionSpread.update EditCollectionSpreadMsg viewConfig msg_
                    |> Tuple.mapFirst
                        (\editCollectionSpread -> { model | editCollectionSpread = editCollectionSpread })

            IndexMsg msg_ ->
                model.index
                    |> View.Index.update IndexMsg viewConfig msg_
                    |> Tuple.mapFirst
                        (\index -> { model | index = index })

            EditBulletMsg msg_ ->
                model.editBullet
                    |> Maybe.map (View.EditBullet.update EditBulletMsg viewConfig msg_)
                    |> Maybe.map
                        (Tuple.mapFirst
                            (\editBullet -> { model | editBullet = Just editBullet })
                        )
                    |> Maybe.withDefault ( model, Cmd.none )

            TodayChanged today ->
                ( { model | today = today }, Cmd.none )

            NowChanged now ->
                ( { model | now = now }, Cmd.none )

            BackClicked ->
                ( model
                , Navigation.newUrl (Url.toString Url.Index)
                )

            TodayClicked ->
                let
                    ( year, month, dayOfMonth ) =
                        Calendar.toGregorian model.today
                in
                    ( model
                    , Task.attempt TodayClickedResult
                        (DailySpread.getBy viewConfig.parse year month dayOfMonth
                            |> Task.andThen
                                (\dailySpread ->
                                    case dailySpread of
                                        Just dailySpread ->
                                            Task.succeed dailySpread.objectId

                                        Nothing ->
                                            DailySpread.create viewConfig.parse
                                                (DailySpread.empty year month dayOfMonth)
                                )
                        )
                    )

            TodayClickedResult (Err err) ->
                ( { model | error = Just err }, Cmd.none )

            TodayClickedResult (Ok dailySpreadId) ->
                ( model
                , Navigation.newUrl (Url.toString (Url.DailySpread dailySpreadId))
                )

            MonthClicked ->
                let
                    ( year, month, _ ) =
                        Calendar.toGregorian model.today
                in
                    ( model
                    , Task.attempt MonthClickedResult
                        (MonthlySpread.getBy viewConfig.parse year month
                            |> Task.andThen
                                (\dailySpread ->
                                    case dailySpread of
                                        Just dailySpread ->
                                            Task.succeed dailySpread.objectId

                                        Nothing ->
                                            MonthlySpread.create viewConfig.parse
                                                (MonthlySpread.empty year month)
                                )
                        )
                    )

            MonthClickedResult (Err err) ->
                ( { model | error = Just err }, Cmd.none )

            MonthClickedResult (Ok monthlySpreadId) ->
                ( model
                , Navigation.newUrl (Url.toString (Url.MonthlySpread monthlySpreadId))
                )


makeViewConfig : Model -> View.Config Msg
makeViewConfig model =
    let
        parseConfig =
            Parse.simpleConfig "bujo" "bujo"
                |> \config ->
                    { config | serverUrl = "http://localhost:1337/parse" }
    in
        { toolbar =
            \config ->
                Toolbar.view Mdc
                    "toolbar"
                    model.mdc
                    [ Toolbar.fixed
                    ]
                    [ Toolbar.row []
                        (List.concat
                            [ [ Toolbar.section
                                    [ Toolbar.alignStart
                                    , Toolbar.shrinkToFit
                                    ]
                                    [ config.menuIcon
                                    , Toolbar.title [] [ text config.title ]
                                    ]
                              ]
                            , [ Toolbar.section
                                    [ Toolbar.alignStart
                                    ]
                                    [ Button.view Mdc
                                        "toolbar__today"
                                        model.mdc
                                        [ Button.onClick MonthClicked
                                        ]
                                        [ text "Month"
                                        ]
                                    , Button.view Mdc
                                        "toolbar__today"
                                        model.mdc
                                        [ Button.onClick TodayClicked
                                        ]
                                        [ text "Today"
                                        ]
                                    ]
                              ]
                            , config.additionalSections
                            ]
                        )
                    ]
        , today = model.today
        , now = model.now
        , parse = parseConfig
        }


view : Model -> Html Msg
view model =
    let
        viewConfig =
            makeViewConfig model
    in
        styled Html.div
            [ cs "main"
            , Toolbar.fixedAdjust "toolbar" model.mdc
            ]
            [ Html.div
                [ Html.class "main__wrapper"
                ]
                [ case model.url of
                    Url.Index ->
                        viewIndex viewConfig model

                    Url.MonthlySpread objectId ->
                        viewMonthlySpread viewConfig model

                    Url.DailySpread objectId ->
                        viewDailySpread viewConfig model

                    Url.CollectionSpread objectId ->
                        viewCollectionSpread viewConfig model

                    Url.EditMonthlySpread objectId ->
                        viewEditMonthlySpread viewConfig model

                    Url.EditDailySpread objectId ->
                        viewEditDailySpread viewConfig model

                    Url.EditCollectionSpread objectId ->
                        viewEditCollectionSpread viewConfig model

                    Url.NotFound urlString ->
                        viewNotFound viewConfig urlString model

                    Url.EditBullet route className objectId bulletId ->
                        viewEditBullet viewConfig model
                ]
            ]


viewMonthlySpread : View.Config Msg -> Model -> Html Msg
viewMonthlySpread viewConfig model =
    View.MonthlySpread.view MonthlySpreadMsg viewConfig model.monthlySpread


viewDailySpread : View.Config Msg -> Model -> Html Msg
viewDailySpread viewConfig model =
    View.DailySpread.view DailySpreadMsg viewConfig model.dailySpread


viewCollectionSpread : View.Config Msg -> Model -> Html Msg
viewCollectionSpread viewConfig model =
    View.CollectionSpread.view CollectionSpreadMsg viewConfig model.collectionSpread


viewEditMonthlySpread : View.Config Msg -> Model -> Html Msg
viewEditMonthlySpread viewConfig model =
    View.EditMonthlySpread.view EditMonthlySpreadMsg viewConfig model.editMonthlySpread


viewEditDailySpread : View.Config Msg -> Model -> Html Msg
viewEditDailySpread viewConfig model =
    View.EditDailySpread.view EditDailySpreadMsg viewConfig model.editDailySpread


viewEditCollectionSpread : View.Config Msg -> Model -> Html Msg
viewEditCollectionSpread viewConfig model =
    View.EditCollectionSpread.view EditCollectionSpreadMsg
        viewConfig
        model.editCollectionSpread


viewNotFound : View.Config Msg -> String -> Model -> Html Msg
viewNotFound viewConfig urlString model =
    Html.div
        [ Html.class "not-found"
        ]
        [ text ("URL not found: " ++ urlString)
        ]


viewIndex : View.Config Msg -> Model -> Html Msg
viewIndex viewConfig model =
    View.Index.view IndexMsg viewConfig model.index


viewEditBullet : View.Config Msg -> Model -> Html Msg
viewEditBullet viewConfig model =
    model.editBullet
        |> Maybe.map (View.EditBullet.view EditBulletMsg viewConfig)
        |> Maybe.withDefault (text "")
