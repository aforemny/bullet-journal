module Screen exposing
    ( Config
    , Msg
    , Screen(..)
    , ToolbarConfig
    , init
    , subscriptions
    , update
    , urlChanged
    , view
    )

import Browser.Navigation
import Html exposing (Html, text)
import Parse
import Route exposing (Route)
import Screen.DailySpread
import Screen.EditBullet
import Screen.MonthlySpread
import Screen.Start
import Time
import Time.Calendar.Days as Calendar


type alias Config msg =
    { topAppBar : ToolbarConfig msg -> Html msg
    , fixedAdjust : Html.Attribute msg
    , today : Calendar.Day
    , now : Time.Posix
    , parse : Parse.Config
    , key : Browser.Navigation.Key
    , timeZone : Time.Zone
    }


type alias ToolbarConfig msg =
    { title : String
    , menuIcon : Maybe (Html msg)
    , additionalSections : List (Html msg)
    }


type Screen
    = Overview Screen.Start.Model
    | DailySpread Screen.DailySpread.Model
    | MonthlySpread Screen.MonthlySpread.Model
    | EditBullet Screen.EditBullet.Model
    | NotFound String


type Msg msg
    = EditBulletMsg (Screen.EditBullet.Msg msg)
    | DailySpreadMsg (Screen.DailySpread.Msg msg)
    | OverviewMsg (Screen.Start.Msg msg)
    | MonthlySpreadMsg (Screen.MonthlySpread.Msg msg)


init : (Msg msg -> msg) -> Config msg -> Route -> ( Screen, Cmd msg )
init lift config route =
    urlChanged lift config Nothing route


urlChanged : (Msg msg -> msg) -> Config msg -> Maybe Route -> Route -> ( Screen, Cmd msg )
urlChanged lift config referringRoute route =
    case route of
        Route.Start ->
            Screen.Start.init (lift << OverviewMsg)
                { key = config.key
                , today = config.today
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                |> Tuple.mapFirst Overview

        Route.DailySpread { year, month, dayOfMonth } ->
            Screen.DailySpread.init (lift << DailySpreadMsg)
                { key = config.key
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                { year = year
                , month = month
                , dayOfMonth = dayOfMonth
                }
                |> Tuple.mapFirst DailySpread

        Route.MonthlySpread { year, month } ->
            Screen.MonthlySpread.init (lift << MonthlySpreadMsg)
                { key = config.key
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                { year = year
                , month = month
                }
                |> Tuple.mapFirst MonthlySpread

        Route.EditBullet maybeBulletId ->
            Screen.EditBullet.init (lift << EditBulletMsg)
                { key = config.key
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                referringRoute
                maybeBulletId
                |> Tuple.mapFirst EditBullet

        Route.NotFound hash ->
            ( NotFound hash, Cmd.none )


subscriptions : (Msg msg -> msg) -> Screen -> Sub msg
subscriptions lift screen =
    case screen of
        Overview overview ->
            Screen.Start.subscriptions (lift << OverviewMsg) overview

        DailySpread dailySpread ->
            Screen.DailySpread.subscriptions (lift << DailySpreadMsg) dailySpread

        MonthlySpread monthlySpread ->
            Screen.MonthlySpread.subscriptions (lift << MonthlySpreadMsg) monthlySpread

        EditBullet editBullet ->
            Screen.EditBullet.subscriptions (lift << EditBulletMsg) editBullet

        NotFound _ ->
            Sub.none


update : (Msg msg -> msg) -> Config msg -> Msg msg -> Screen -> ( Screen, Cmd msg )
update lift config msg screen =
    case msg of
        MonthlySpreadMsg msg_ ->
            case screen of
                MonthlySpread monthlySpread ->
                    Screen.MonthlySpread.update (lift << MonthlySpreadMsg)
                        { key = config.key
                        , parse = config.parse
                        , topAppBar = config.topAppBar
                        , fixedAdjust = config.fixedAdjust
                        }
                        msg_
                        monthlySpread
                        |> Tuple.mapFirst MonthlySpread

                _ ->
                    ( screen, Cmd.none )

        DailySpreadMsg msg_ ->
            case screen of
                DailySpread dailySpread ->
                    Screen.DailySpread.update (lift << DailySpreadMsg)
                        { key = config.key
                        , parse = config.parse
                        , topAppBar = config.topAppBar
                        , fixedAdjust = config.fixedAdjust
                        }
                        msg_
                        dailySpread
                        |> Tuple.mapFirst DailySpread

                _ ->
                    ( screen, Cmd.none )

        OverviewMsg msg_ ->
            case screen of
                Overview overview ->
                    Screen.Start.update (lift << OverviewMsg)
                        { key = config.key
                        , today = config.today
                        , parse = config.parse
                        , topAppBar = config.topAppBar
                        , fixedAdjust = config.fixedAdjust
                        }
                        msg_
                        overview
                        |> Tuple.mapFirst Overview

                _ ->
                    ( screen, Cmd.none )

        EditBulletMsg msg_ ->
            case screen of
                EditBullet editBullet ->
                    Screen.EditBullet.update (lift << EditBulletMsg)
                        { key = config.key
                        , parse = config.parse
                        , topAppBar = config.topAppBar
                        , fixedAdjust = config.fixedAdjust
                        }
                        msg_
                        editBullet
                        |> Tuple.mapFirst EditBullet

                _ ->
                    ( screen, Cmd.none )


view : (Msg msg -> msg) -> Config msg -> Screen -> List (Html msg)
view lift config screen =
    case screen of
        Overview overview ->
            Screen.Start.view (lift << OverviewMsg)
                { key = config.key
                , today = config.today
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                overview

        MonthlySpread monthlySpread ->
            Screen.MonthlySpread.view (lift << MonthlySpreadMsg)
                { key = config.key
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                monthlySpread

        DailySpread monthlySpread ->
            Screen.DailySpread.view (lift << DailySpreadMsg)
                { key = config.key
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                monthlySpread

        EditBullet editBullet ->
            Screen.EditBullet.view (lift << EditBulletMsg)
                { key = config.key
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                editBullet

        NotFound notFound ->
            [ text <| "notFound: " ++ notFound ]
