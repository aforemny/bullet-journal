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
import Screen
import Screen.CollectionSpread
import Screen.DailySpread
import Screen.EditBullet
import Screen.EditCollectionSpread
import Screen.EditDailySpread
import Screen.EditMonthlySpread
import Screen.Index
import Screen.MonthlySpread
import Screen.Start
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


type alias Model =
    { key : Browser.Navigation.Key
    , url : Route
    , collectionSpread : Screen.CollectionSpread.Model
    , dailySpread : Screen.DailySpread.Model
    , index : Screen.Index.Model
    , monthlySpread : Screen.MonthlySpread.Model
    , editCollectionSpread : Screen.EditCollectionSpread.Model
    , editDailySpread : Screen.EditDailySpread.Model
    , editMonthlySpread : Screen.EditMonthlySpread.Model
    , editBullet : Maybe Screen.EditBullet.Model
    , start : Screen.Start.Model
    , today : Calendar.Day
    , now : Time.Posix
    , error : Maybe Parse.Error
    , timeZone : Time.Zone
    }


defaultModel : Browser.Navigation.Key -> Model
defaultModel key =
    { url = Route.Index
    , collectionSpread = Screen.CollectionSpread.defaultModel
    , dailySpread = Screen.DailySpread.defaultModel
    , index = Screen.Index.defaultModel
    , monthlySpread = Screen.MonthlySpread.defaultModel
    , editMonthlySpread = Screen.EditMonthlySpread.defaultModel
    , editCollectionSpread = Screen.EditCollectionSpread.defaultModel
    , editDailySpread = Screen.EditDailySpread.defaultModel
    , editBullet = Nothing
    , start = Screen.Start.defaultModel
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
    | EditBulletMsg (Screen.EditBullet.Msg Msg)
    | CollectionSpreadMsg (Screen.CollectionSpread.Msg Msg)
    | DailySpreadMsg (Screen.DailySpread.Msg Msg)
    | IndexMsg (Screen.Index.Msg Msg)
    | StartMsg (Screen.Start.Msg Msg)
    | MonthlySpreadMsg (Screen.MonthlySpread.Msg Msg)
    | EditCollectionSpreadMsg (Screen.EditCollectionSpread.Msg Msg)
    | EditDailySpreadMsg (Screen.EditDailySpread.Msg Msg)
    | EditMonthlySpreadMsg (Screen.EditMonthlySpread.Msg Msg)
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

        screenConfig =
            makeScreenConfig model

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
        |> andThenInitScreen screenConfig url


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.today TodayChanged
        , Ports.now NowChanged
        , Screen.CollectionSpread.subscriptions CollectionSpreadMsg model.collectionSpread
        , Screen.DailySpread.subscriptions DailySpreadMsg model.dailySpread
        , Screen.Index.subscriptions IndexMsg model.index
        , Screen.MonthlySpread.subscriptions MonthlySpreadMsg model.monthlySpread
        , Screen.EditMonthlySpread.subscriptions EditMonthlySpreadMsg model.editMonthlySpread
        , Screen.EditCollectionSpread.subscriptions EditCollectionSpreadMsg model.editCollectionSpread
        , Screen.Start.subscriptions StartMsg model.start
        , Screen.EditDailySpread.subscriptions EditDailySpreadMsg model.editDailySpread
        , model.editBullet
            |> Maybe.map (Screen.EditBullet.subscriptions EditBulletMsg)
            |> Maybe.withDefault Sub.none
        ]


andThenInitScreen : Screen.Config Msg -> Route -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThenInitScreen screenConfig url ( model, cmd ) =
    (case url of
        Route.Start ->
            Screen.Start.init StartMsg screenConfig model.start
                |> Tuple.mapFirst (\start -> { model | start = start })

        Route.Index ->
            Screen.Index.init IndexMsg screenConfig model.index
                |> Tuple.mapFirst (\index -> { model | index = index })

        Route.MonthlySpread objectId ->
            Screen.MonthlySpread.init MonthlySpreadMsg
                screenConfig
                objectId
                model.monthlySpread
                |> Tuple.mapFirst
                    (\monthlySpread ->
                        { model | monthlySpread = monthlySpread }
                    )

        Route.DailySpread objectId ->
            Screen.DailySpread.init
                DailySpreadMsg
                screenConfig
                objectId
                model.dailySpread
                |> Tuple.mapFirst
                    (\dailySpread ->
                        { model | dailySpread = dailySpread }
                    )

        Route.CollectionSpread objectId ->
            Screen.CollectionSpread.init CollectionSpreadMsg
                screenConfig
                objectId
                model.collectionSpread
                |> Tuple.mapFirst
                    (\collectionSpread ->
                        { model | collectionSpread = collectionSpread }
                    )

        Route.EditMonthlySpread objectId ->
            Screen.EditMonthlySpread.init EditMonthlySpreadMsg
                screenConfig
                objectId
                model.editMonthlySpread
                |> Tuple.mapFirst
                    (\editMonthlySpread ->
                        { model | editMonthlySpread = editMonthlySpread }
                    )

        Route.EditDailySpread objectId ->
            Screen.EditDailySpread.init
                EditDailySpreadMsg
                screenConfig
                objectId
                model.editDailySpread
                |> Tuple.mapFirst
                    (\editDailySpread ->
                        { model | editDailySpread = editDailySpread }
                    )

        Route.EditCollectionSpread objectId ->
            Screen.EditCollectionSpread.init EditCollectionSpreadMsg
                screenConfig
                objectId
                model.editCollectionSpread
                |> Tuple.mapFirst
                    (\editCollectionSpread ->
                        { model | editCollectionSpread = editCollectionSpread }
                    )

        Route.EditBullet bulletId ->
            Screen.EditBullet.init EditBulletMsg
                screenConfig
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
        screenConfig =
            makeScreenConfig model
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MonthlySpreadMsg msg_ ->
            model.monthlySpread
                |> Screen.MonthlySpread.update MonthlySpreadMsg screenConfig msg_
                |> Tuple.mapFirst
                    (\monthlySpread -> { model | monthlySpread = monthlySpread })

        DailySpreadMsg msg_ ->
            model.dailySpread
                |> Screen.DailySpread.update DailySpreadMsg screenConfig msg_
                |> Tuple.mapFirst
                    (\dailySpread -> { model | dailySpread = dailySpread })

        CollectionSpreadMsg msg_ ->
            model.collectionSpread
                |> Screen.CollectionSpread.update CollectionSpreadMsg screenConfig msg_
                |> Tuple.mapFirst
                    (\collectionSpread -> { model | collectionSpread = collectionSpread })

        EditMonthlySpreadMsg msg_ ->
            model.editMonthlySpread
                |> Screen.EditMonthlySpread.update EditMonthlySpreadMsg screenConfig msg_
                |> Tuple.mapFirst
                    (\editMonthlySpread -> { model | editMonthlySpread = editMonthlySpread })

        EditDailySpreadMsg msg_ ->
            model.editDailySpread
                |> Screen.EditDailySpread.update EditDailySpreadMsg screenConfig msg_
                |> Tuple.mapFirst
                    (\editDailySpread -> { model | editDailySpread = editDailySpread })

        EditCollectionSpreadMsg msg_ ->
            model.editCollectionSpread
                |> Screen.EditCollectionSpread.update EditCollectionSpreadMsg screenConfig msg_
                |> Tuple.mapFirst
                    (\editCollectionSpread -> { model | editCollectionSpread = editCollectionSpread })

        IndexMsg msg_ ->
            model.index
                |> Screen.Index.update IndexMsg screenConfig msg_
                |> Tuple.mapFirst
                    (\index -> { model | index = index })

        StartMsg msg_ ->
            model.start
                |> Screen.Start.update StartMsg screenConfig msg_
                |> Tuple.mapFirst
                    (\start -> { model | start = start })

        EditBulletMsg msg_ ->
            model.editBullet
                |> Maybe.map (Screen.EditBullet.update EditBulletMsg screenConfig msg_)
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
                (DailySpread.getBy screenConfig.parse year month dayOfMonth
                    |> Task.andThen
                        (\dailySpread_ ->
                            case dailySpread_ of
                                Just dailySpread ->
                                    Task.succeed dailySpread.objectId

                                Nothing ->
                                    DailySpread.create screenConfig.parse
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
                (MonthlySpread.getBy screenConfig.parse year month
                    |> Task.andThen
                        (\dailySpread_ ->
                            case dailySpread_ of
                                Just dailySpread ->
                                    Task.succeed dailySpread.objectId

                                Nothing ->
                                    MonthlySpread.create screenConfig.parse
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


makeScreenConfig : Model -> Screen.Config Msg
makeScreenConfig model =
    { topAppBar =
        \config ->
            topAppBar { topAppBarConfig | fixed = True }
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
    , fixedAdjust = TopAppBar.fixedAdjust
    , today = model.today
    , now = model.now
    , parse = parseConfig
    , key = model.key
    , timeZone = model.timeZone
    }


view model =
    let
        screenConfig =
            makeScreenConfig model
    in
    { title = "Bujo"
    , body =
        [ Html.div [ class "main" ]
            (case model.url of
                Route.Start ->
                    viewStart screenConfig model

                Route.Index ->
                    viewIndex screenConfig model

                Route.MonthlySpread objectId ->
                    viewMonthlySpread screenConfig model

                Route.DailySpread objectId ->
                    viewDailySpread screenConfig model

                Route.CollectionSpread objectId ->
                    viewCollectionSpread screenConfig model

                Route.EditMonthlySpread objectId ->
                    viewEditMonthlySpread screenConfig model

                Route.EditDailySpread objectId ->
                    viewEditDailySpread screenConfig model

                Route.EditCollectionSpread objectId ->
                    viewEditCollectionSpread screenConfig model

                Route.NotFound urlString ->
                    viewNotFound screenConfig urlString model

                Route.EditBullet _ ->
                    viewEditBullet screenConfig model
            )
        ]
    }


viewMonthlySpread : Screen.Config Msg -> Model -> List (Html Msg)
viewMonthlySpread screenConfig model =
    Screen.MonthlySpread.view MonthlySpreadMsg screenConfig model.monthlySpread


viewDailySpread : Screen.Config Msg -> Model -> List (Html Msg)
viewDailySpread screenConfig model =
    Screen.DailySpread.view DailySpreadMsg screenConfig model.dailySpread


viewCollectionSpread : Screen.Config Msg -> Model -> List (Html Msg)
viewCollectionSpread screenConfig model =
    Screen.CollectionSpread.view CollectionSpreadMsg screenConfig model.collectionSpread


viewEditMonthlySpread : Screen.Config Msg -> Model -> List (Html Msg)
viewEditMonthlySpread screenConfig model =
    Screen.EditMonthlySpread.view EditMonthlySpreadMsg screenConfig model.editMonthlySpread


viewEditDailySpread : Screen.Config Msg -> Model -> List (Html Msg)
viewEditDailySpread screenConfig model =
    Screen.EditDailySpread.view EditDailySpreadMsg screenConfig model.editDailySpread


viewEditCollectionSpread : Screen.Config Msg -> Model -> List (Html Msg)
viewEditCollectionSpread screenConfig model =
    Screen.EditCollectionSpread.view EditCollectionSpreadMsg
        screenConfig
        model.editCollectionSpread


viewNotFound : Screen.Config Msg -> String -> Model -> List (Html Msg)
viewNotFound screenConfig urlString model =
    [ Html.div [ class "not-found" ]
        [ text ("URL not found: " ++ urlString) ]
    ]


viewIndex : Screen.Config Msg -> Model -> List (Html Msg)
viewIndex screenConfig model =
    Screen.Index.view IndexMsg screenConfig model.index


viewStart : Screen.Config Msg -> Model -> List (Html Msg)
viewStart screenConfig model =
    Screen.Start.view StartMsg screenConfig model.start


viewEditBullet : Screen.Config Msg -> Model -> List (Html Msg)
viewEditBullet screenConfig model =
    model.editBullet
        |> Maybe.map (Screen.EditBullet.view EditBulletMsg screenConfig)
        |> Maybe.withDefault []
