module Main exposing (main)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode exposing (Value)
import Material.Button exposing (buttonConfig, textButton)
import Material.Drawer exposing (drawerScrim, modalDrawer, modalDrawerConfig)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.List exposing (list, listConfig, listItem, listItemConfig, listItemGraphic)
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Parse
import Parse.Private.ObjectId as ObjectId
import ParseConfig exposing (parseConfig)
import Ports
import Route exposing (Route)
import Screen exposing (Screen)
import Screen.DailySpread
import Screen.EditBullet
import Screen.MonthlySpread
import Screen.Overview
import Task exposing (Task)
import Time
import Time.Calendar.Days as Calendar
import Time.Calendar.Gregorian as Calendar
import Time.Format.Locale as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Url exposing (Url)


type alias Model =
    { key : Browser.Navigation.Key
    , route : Route
    , today : Calendar.Day
    , now : Time.Posix
    , timeZone : Time.Zone
    , drawerOpen : Bool
    , screen : Screen
    }


defaultModel : Browser.Navigation.Key -> Model
defaultModel key =
    { key = key
    , route = Route.Overview
    , today = Calendar.fromGregorian 1970 1 1
    , now = Time.millisToPosix 0
    , timeZone = Time.utc
    , drawerOpen = False
    , screen = Screen.Overview Screen.Overview.defaultModel
    }


type Msg
    = NoOp
    | TodayChanged (Maybe Calendar.Day)
    | NowChanged (Maybe Time.Posix)
    | BackClicked
    | TodayClicked
    | ThisMonthClicked
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | StartClicked
    | TableOfContentClicked
    | DrawerClosed
    | OpenDrawerClicked
    | ScreenMsg (Screen.Msg Msg)


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
init flags url key =
    let
        route =
            Route.fromUrl url

        ( model, cmd ) =
            ( defaultModel key
                |> (\model_ ->
                        { model_
                            | today =
                                Ports.readDayUnsafe flags.today
                                    |> Maybe.withDefault model_.today
                            , now =
                                Ports.readDateUnsafe flags.now
                                    |> Maybe.withDefault model_.now
                            , route = route
                        }
                   )
            , Cmd.none
            )
    in
    Screen.init ScreenMsg (makeScreenConfig model) route
        |> Tuple.mapFirst (\screen -> { model | screen = screen })
        |> Tuple.mapSecond (\screenCmd -> Cmd.batch [ cmd, screenCmd ])


urlChanged : Route -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
urlChanged route ( model, cmd ) =
    Screen.urlChanged ScreenMsg (makeScreenConfig model) (Just model.route) route
        |> Tuple.mapFirst (\screen -> { model | route = route, screen = screen })
        |> Tuple.mapSecond (\screenCmd -> Cmd.batch [ cmd, screenCmd ])


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.today TodayChanged
        , Ports.now NowChanged
        , Screen.subscriptions ScreenMsg model.screen
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        screenConfig =
            makeScreenConfig model
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TodayChanged Nothing ->
            ( model, Cmd.none )

        TodayChanged (Just today) ->
            ( { model | today = today }, Cmd.none )

        NowChanged Nothing ->
            ( model, Cmd.none )

        NowChanged (Just now) ->
            ( { model | now = now }, Cmd.none )

        BackClicked ->
            ( model, Browser.Navigation.pushUrl model.key (Route.toString Route.Overview) )

        TodayClicked ->
            let
                ( year, month, dayOfMonth ) =
                    Calendar.toGregorian model.today
            in
            ( model
            , Browser.Navigation.pushUrl model.key
                (Route.toString
                    (Route.DailySpread
                        { year = year
                        , month = month
                        , dayOfMonth = dayOfMonth
                        }
                    )
                )
            )

        ThisMonthClicked ->
            let
                ( year, month, _ ) =
                    Calendar.toGregorian model.today
            in
            ( model
            , Browser.Navigation.pushUrl model.key
                (Route.toString (Route.MonthlySpread { year = year, month = month }))
            )

        UrlRequested (Browser.Internal url) ->
            ( model
            , Browser.Navigation.pushUrl model.key (Route.toString (Route.fromUrl url))
            )

        UrlRequested (Browser.External url) ->
            ( model, Browser.Navigation.load url )

        UrlChanged url ->
            urlChanged (Route.fromUrl url) ( model, Cmd.none )

        StartClicked ->
            ( model
            , Browser.Navigation.pushUrl model.key (Route.toString Route.Overview)
            )

        TableOfContentClicked ->
            ( model
            , Browser.Navigation.pushUrl model.key (Route.toString Route.TableOfContent)
            )

        DrawerClosed ->
            ( { model | drawerOpen = False }, Cmd.none )

        OpenDrawerClicked ->
            ( { model | drawerOpen = True }, Cmd.none )

        ScreenMsg msg_ ->
            Screen.update ScreenMsg screenConfig msg_ model.screen
                |> Tuple.mapFirst (\screen -> { model | screen = screen })


view model =
    let
        screenConfig =
            makeScreenConfig model
    in
    { title = "Bujo"
    , body =
        [ Html.div [ class "main" ]
            (drawer model ++ content screenConfig model)
        ]
    }


content screenConfig model =
    [ Html.div [ class "main__content" ]
        (Screen.view ScreenMsg screenConfig model.screen)
    ]


drawer model =
    let
        ( year, month, dayOfMonth ) =
            Calendar.toGregorian model.today
    in
    [ modalDrawer
        { modalDrawerConfig
            | open = model.drawerOpen
            , onClose = Just DrawerClosed
        }
        [ list listConfig
            (List.map
                (\{ label, activated, onClick } ->
                    listItem
                        { listItemConfig
                            | activated = activated
                            , onClick = Just onClick
                        }
                        [ text label ]
                )
                [ { label = "Overview"
                  , activated = model.route == Route.Overview
                  , onClick = StartClicked
                  }
                , { label = "Table of Content"
                  , activated = model.route == Route.TableOfContent
                  , onClick = TableOfContentClicked
                  }
                , { label = "Today"
                  , activated =
                        model.route
                            == Route.DailySpread
                                { year = year, month = month, dayOfMonth = dayOfMonth }
                  , onClick = TodayClicked
                  }
                , { label = "This month"
                  , activated =
                        model.route == Route.MonthlySpread { year = year, month = month }
                  , onClick = ThisMonthClicked
                  }
                ]
            )
        ]
    , drawerScrim [] []
    ]


topAppBar_ config =
    topAppBar { topAppBarConfig | fixed = True }
        [ TopAppBar.row []
            ([ TopAppBar.section [ TopAppBar.alignStart ]
                [ config.menuIcon
                    |> Maybe.withDefault
                        (iconButton
                            { iconButtonConfig
                                | onClick = Just OpenDrawerClicked
                                , additionalAttributes =
                                    [ TopAppBar.navigationIcon ]
                            }
                            "menu"
                        )
                , Html.h1 [ TopAppBar.title ] [ text config.title ]
                ]
             ]
                ++ config.additionalSections
            )
        ]


makeScreenConfig : Model -> Screen.Config Msg
makeScreenConfig model =
    { topAppBar = topAppBar_
    , fixedAdjust = TopAppBar.fixedAdjust
    , today = model.today
    , now = model.now
    , parse = parseConfig
    , key = model.key
    , timeZone = model.timeZone
    }
