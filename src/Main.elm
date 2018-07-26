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
import View.Index
import View.MonthlySpread
import View.NewBullet


type alias Model =
    { mdc : Material.Model Msg
    , url : Url
    , collectionSpread : View.CollectionSpread.Model Msg
    , dailySpread : View.DailySpread.Model Msg
    , index : View.Index.Model Msg
    , monthlySpread : View.MonthlySpread.Model Msg
    , newBullet : Maybe (View.NewBullet.Model Msg)
    , today : Calendar.Day
    , now : Date
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , url = Url.Index
    , collectionSpread = View.CollectionSpread.defaultModel
    , dailySpread = View.DailySpread.defaultModel
    , index = View.Index.defaultModel
    , monthlySpread = View.MonthlySpread.defaultModel
    , newBullet = Nothing
    , today = Calendar.fromGregorian 1970 1 1
    , now = Date.fromTime 0
    }


type Msg
    = NoOp
    | Mdc (Material.Msg Msg)
    | SetUrl Url
    | TodayChanged Calendar.Day
    | NowChanged Date
    | BackClicked
    | NewBulletMsg (View.NewBullet.Msg Msg)
    | CollectionSpreadMsg (View.CollectionSpread.Msg Msg)
    | DailySpreadMsg (View.DailySpread.Msg Msg)
    | IndexMsg (View.Index.Msg Msg)
    | MonthlySpreadMsg (View.MonthlySpread.Msg Msg)


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
        , model.newBullet
            |> Maybe.map (View.NewBullet.subscriptions NewBulletMsg)
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

        Url.NewBullet route className objectId ->
            View.NewBullet.init NewBulletMsg
                viewConfig
                (case className of
                    "CollectionSpread" ->
                        Url.CollectionSpread (Bullet.castObjectId objectId)

                    "MonthlySpread" ->
                        Url.MonthlySpread (Bullet.castObjectId objectId)

                    _ ->
                        Url.DailySpread (Bullet.castObjectId objectId)
                )
                className
                objectId
                model.newBullet
                |> Tuple.mapFirst
                    (\newBullet ->
                        { model | newBullet = Just newBullet }
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

            IndexMsg msg_ ->
                model.index
                    |> View.Index.update IndexMsg viewConfig msg_
                    |> Tuple.mapFirst
                        (\index -> { model | index = index })

            NewBulletMsg msg_ ->
                model.newBullet
                    |> Maybe.map (View.NewBullet.update NewBulletMsg viewConfig msg_)
                    |> Maybe.map
                        (Tuple.mapFirst
                            (\newBullet -> { model | newBullet = Just newBullet })
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
                    []
                    [ Toolbar.row []
                        (Toolbar.section
                            [ Toolbar.alignStart
                            ]
                            [ if model.url == Url.Index then
                                Icon.view
                                    [ Toolbar.menuIcon
                                    ]
                                    "menu"
                              else
                                Icon.view
                                    [ Toolbar.menuIcon
                                    , Options.onClick BackClicked
                                    ]
                                    "arrow_back"
                            , Toolbar.title [] [ text "bujo" ]
                            ]
                            :: config.additionalSections
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
        Html.div
            [ Html.class "main"
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

                    Url.NotFound urlString ->
                        viewNotFound viewConfig urlString model

                    Url.NewBullet route className objectId ->
                        viewNewBullet viewConfig model
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


viewNotFound : View.Config Msg -> String -> Model -> Html Msg
viewNotFound viewConfig urlString model =
    Html.div
        [ Html.class "not-found"
        ]
        [ viewConfig.toolbar
            { additionalSections = []
            }
        , text ("URL not found: " ++ urlString)
        ]


viewIndex : View.Config Msg -> Model -> Html Msg
viewIndex viewConfig model =
    View.Index.view IndexMsg viewConfig model.index


viewNewBullet : View.Config Msg -> Model -> Html Msg
viewNewBullet viewConfig model =
    model.newBullet
        |> Maybe.map (View.NewBullet.view NewBulletMsg viewConfig)
        |> Maybe.withDefault (text "")
