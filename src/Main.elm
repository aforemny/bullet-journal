module Main exposing (main)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode exposing (Value)
import Material.Button exposing (buttonConfig, textButton)
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Parse
import Parse.Private.ObjectId as ObjectId
import ParseConfig exposing (parseConfig)
import Ports
import Route exposing (Route)
import Task exposing (Task)
import Time
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
import View.Start


type alias Model =
    { key : Browser.Navigation.Key
    , url : Route
    , collectionSpread : View.CollectionSpread.Model
    , dailySpread : View.DailySpread.Model
    , index : View.Index.Model
    , monthlySpread : View.MonthlySpread.Model
    , editCollectionSpread : View.EditCollectionSpread.Model
    , editDailySpread : View.EditDailySpread.Model
    , editMonthlySpread : View.EditMonthlySpread.Model
    , editBullet : Maybe View.EditBullet.Model
    , start : View.Start.Model
    , today : Calendar.Day
    , now : Time.Posix
    , error : Maybe Parse.Error
    , timeZone : Time.Zone
    }


defaultModel : Browser.Navigation.Key -> Model
defaultModel key =
    { url = Route.Index
    , collectionSpread = View.CollectionSpread.defaultModel
    , dailySpread = View.DailySpread.defaultModel
    , index = View.Index.defaultModel
    , monthlySpread = View.MonthlySpread.defaultModel
    , editMonthlySpread = View.EditMonthlySpread.defaultModel
    , editCollectionSpread = View.EditCollectionSpread.defaultModel
    , editDailySpread = View.EditDailySpread.defaultModel
    , editBullet = Nothing
    , start = View.Start.defaultModel
    , today = Calendar.fromGregorian 1970 1 1
    , now = Time.millisToPosix 0
    , error = Nothing
    , timeZone = Time.utc
    , key = key
    }


type Msg
    = NoOp
    | TodayChanged (Maybe Calendar.Day)
    | NowChanged (Maybe Time.Posix)
    | BackClicked
    | EditBulletMsg (View.EditBullet.Msg Msg)
    | CollectionSpreadMsg (View.CollectionSpread.Msg Msg)
    | DailySpreadMsg (View.DailySpread.Msg Msg)
    | IndexMsg (View.Index.Msg Msg)
    | StartMsg (View.Start.Msg Msg)
    | MonthlySpreadMsg (View.MonthlySpread.Msg Msg)
    | EditCollectionSpreadMsg (View.EditCollectionSpread.Msg Msg)
    | EditDailySpreadMsg (View.EditDailySpread.Msg Msg)
    | EditMonthlySpreadMsg (View.EditMonthlySpread.Msg Msg)
    | TodayClicked
    | TodayClickedResult (Result Parse.Error (Parse.ObjectId DailySpread))
    | MonthClicked
    | MonthClickedResult (Result Parse.Error (Parse.ObjectId MonthlySpread))
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url


type alias Flags =
    { today : String
    , now : String
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url_ key =
    let
        url =
            Route.fromUrl url_

        viewConfig =
            makeViewConfig model

        model =
            defaultModel key
                |> (\model_ ->
                        { model_
                            | today =
                                Ports.readDayUnsafe flags.today
                                    |> Maybe.withDefault model_.today
                            , now =
                                Ports.readDateUnsafe flags.now
                                    |> Maybe.withDefault model_.now
                            , url = url
                        }
                   )
    in
    ( model, Cmd.none )
        |> initView viewConfig url


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.today TodayChanged
        , Ports.now NowChanged
        , View.CollectionSpread.subscriptions CollectionSpreadMsg model.collectionSpread
        , View.DailySpread.subscriptions DailySpreadMsg model.dailySpread
        , View.Index.subscriptions IndexMsg model.index
        , View.MonthlySpread.subscriptions MonthlySpreadMsg model.monthlySpread
        , View.EditMonthlySpread.subscriptions EditMonthlySpreadMsg model.editMonthlySpread
        , View.EditCollectionSpread.subscriptions EditCollectionSpreadMsg model.editCollectionSpread
        , View.Start.subscriptions StartMsg model.start
        , View.EditDailySpread.subscriptions EditDailySpreadMsg model.editDailySpread
        , model.editBullet
            |> Maybe.map (View.EditBullet.subscriptions EditBulletMsg)
            |> Maybe.withDefault Sub.none
        ]


initView : View.Config Msg -> Route -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initView viewConfig url ( model, cmd ) =
    (case url of
        Route.Start ->
            View.Start.init StartMsg viewConfig model.start
                |> Tuple.mapFirst (\start -> { model | start = start })

        Route.Index ->
            View.Index.init IndexMsg viewConfig model.index
                |> Tuple.mapFirst (\index -> { model | index = index })

        Route.MonthlySpread objectId ->
            View.MonthlySpread.init MonthlySpreadMsg
                viewConfig
                objectId
                model.monthlySpread
                |> Tuple.mapFirst
                    (\monthlySpread ->
                        { model | monthlySpread = monthlySpread }
                    )

        Route.DailySpread objectId ->
            View.DailySpread.init
                DailySpreadMsg
                viewConfig
                objectId
                model.dailySpread
                |> Tuple.mapFirst
                    (\dailySpread ->
                        { model | dailySpread = dailySpread }
                    )

        Route.CollectionSpread objectId ->
            View.CollectionSpread.init CollectionSpreadMsg
                viewConfig
                objectId
                model.collectionSpread
                |> Tuple.mapFirst
                    (\collectionSpread ->
                        { model | collectionSpread = collectionSpread }
                    )

        Route.EditMonthlySpread objectId ->
            View.EditMonthlySpread.init EditMonthlySpreadMsg
                viewConfig
                objectId
                model.editMonthlySpread
                |> Tuple.mapFirst
                    (\editMonthlySpread ->
                        { model | editMonthlySpread = editMonthlySpread }
                    )

        Route.EditDailySpread objectId ->
            View.EditDailySpread.init
                EditDailySpreadMsg
                viewConfig
                objectId
                model.editDailySpread
                |> Tuple.mapFirst
                    (\editDailySpread ->
                        { model | editDailySpread = editDailySpread }
                    )

        Route.EditCollectionSpread objectId ->
            View.EditCollectionSpread.init EditCollectionSpreadMsg
                viewConfig
                objectId
                model.editCollectionSpread
                |> Tuple.mapFirst
                    (\editCollectionSpread ->
                        { model | editCollectionSpread = editCollectionSpread }
                    )

        Route.EditBullet bulletId ->
            View.EditBullet.init EditBulletMsg
                viewConfig
                -- TODO:
                Route.Start
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

        StartMsg msg_ ->
            model.start
                |> View.Start.update StartMsg viewConfig msg_
                |> Tuple.mapFirst
                    (\start -> { model | start = start })

        EditBulletMsg msg_ ->
            model.editBullet
                |> Maybe.map (View.EditBullet.update EditBulletMsg viewConfig msg_)
                |> Maybe.map
                    (Tuple.mapFirst
                        (\editBullet -> { model | editBullet = Just editBullet })
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        TodayChanged Nothing ->
            ( model, Cmd.none )

        TodayChanged (Just today) ->
            ( { model | today = today }, Cmd.none )

        NowChanged Nothing ->
            ( model, Cmd.none )

        NowChanged (Just now) ->
            ( { model | now = now }, Cmd.none )

        BackClicked ->
            ( model, Browser.Navigation.pushUrl model.key (Route.toString Route.Start) )

        TodayClicked ->
            let
                ( year, month, dayOfMonth ) =
                    Calendar.toGregorian model.today
            in
            ( model
            , Task.attempt TodayClickedResult
                (DailySpread.getBy viewConfig.parse year month dayOfMonth
                    |> Task.andThen
                        (\dailySpread_ ->
                            case dailySpread_ of
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
            , Browser.Navigation.pushUrl model.key
                (Route.toString (Route.DailySpread dailySpreadId))
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
                        (\dailySpread_ ->
                            case dailySpread_ of
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
            , Browser.Navigation.pushUrl model.key
                (Route.toString (Route.MonthlySpread monthlySpreadId))
            )

        UrlRequested (Browser.Internal url) ->
            ( model
            , Browser.Navigation.pushUrl model.key (Route.toString (Route.fromUrl url))
            )

        UrlRequested (Browser.External url) ->
            ( model, Browser.Navigation.load url )

        UrlChanged url ->
            ( { model | url = Route.fromUrl url }, Cmd.none )


makeViewConfig : Model -> View.Config Msg
makeViewConfig model =
    { toolbar =
        \config ->
            topAppBar topAppBarConfig
                [ TopAppBar.row []
                    (List.concat
                        [ [ TopAppBar.section [ TopAppBar.alignStart ]
                                [ config.menuIcon
                                , Html.h1 [ TopAppBar.title ] [ text config.title ]
                                ]
                          ]
                        , [ TopAppBar.section [ TopAppBar.alignStart ]
                                [ textButton
                                    { buttonConfig
                                        | onClick = Just MonthClicked
                                    }
                                    "Month"
                                , textButton
                                    { buttonConfig
                                        | onClick = Just TodayClicked
                                    }
                                    "Today"
                                ]
                          ]
                        , config.additionalSections
                        ]
                    )
                ]
    , today = model.today
    , now = model.now
    , parse = parseConfig
    , key = model.key
    , timeZone = model.timeZone
    }


view model =
    let
        viewConfig =
            makeViewConfig model
    in
    { title = "Bujo"
    , body =
        [ Html.div
            [ class "main"
            , TopAppBar.fixedAdjust
            ]
            [ case model.url of
                Route.Start ->
                    viewStart viewConfig model

                Route.Index ->
                    viewIndex viewConfig model

                Route.MonthlySpread objectId ->
                    viewMonthlySpread viewConfig model

                Route.DailySpread objectId ->
                    viewDailySpread viewConfig model

                Route.CollectionSpread objectId ->
                    viewCollectionSpread viewConfig model

                Route.EditMonthlySpread objectId ->
                    viewEditMonthlySpread viewConfig model

                Route.EditDailySpread objectId ->
                    viewEditDailySpread viewConfig model

                Route.EditCollectionSpread objectId ->
                    viewEditCollectionSpread viewConfig model

                Route.NotFound urlString ->
                    viewNotFound viewConfig urlString model

                Route.EditBullet _ ->
                    viewEditBullet viewConfig model
            ]
        ]
    }


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
    Html.div [ class "not-found" ]
        [ text ("URL not found: " ++ urlString) ]


viewIndex : View.Config Msg -> Model -> Html Msg
viewIndex viewConfig model =
    View.Index.view IndexMsg viewConfig model.index


viewStart : View.Config Msg -> Model -> Html Msg
viewStart viewConfig model =
    View.Start.view StartMsg viewConfig model.start


viewEditBullet : View.Config Msg -> Model -> Html Msg
viewEditBullet viewConfig model =
    model.editBullet
        |> Maybe.map (View.EditBullet.view EditBulletMsg viewConfig)
        |> Maybe.withDefault (text "")
