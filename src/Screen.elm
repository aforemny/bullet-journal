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
import Screen.CollectionSpread
import Screen.DailySpread
import Screen.EditBullet
import Screen.EditCollectionSpread
import Screen.MonthlySpread
import Screen.Start
import Screen.TableOfContent
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
    | TableOfContent Screen.TableOfContent.Model
    | DailySpread Screen.DailySpread.Model
    | CollectionSpread Screen.CollectionSpread.Model
    | MonthlySpread Screen.MonthlySpread.Model
    | EditBullet Screen.EditBullet.Model
    | EditCollectionSpread Screen.EditCollectionSpread.Model
    | NotFound String


type Msg msg
    = OverviewMsg (Screen.Start.Msg msg)
    | TableOfContentMsg (Screen.TableOfContent.Msg msg)
    | EditBulletMsg (Screen.EditBullet.Msg msg)
    | EditCollectionSpreadMsg (Screen.EditCollectionSpread.Msg msg)
    | CollectionSpreadMsg (Screen.CollectionSpread.Msg msg)
    | DailySpreadMsg (Screen.DailySpread.Msg msg)
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

        Route.TableOfContent ->
            Screen.TableOfContent.init (lift << TableOfContentMsg)
                { key = config.key
                , today = config.today
                , timeZone = config.timeZone
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                |> Tuple.mapFirst TableOfContent

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

        Route.CollectionSpread collectionSpreadId ->
            Screen.CollectionSpread.init (lift << CollectionSpreadMsg)
                { key = config.key
                , today = config.today
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                collectionSpreadId
                |> Tuple.mapFirst CollectionSpread

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

        Route.EditCollectionSpread maybeCollectionSpreadId ->
            Screen.EditCollectionSpread.init (lift << EditCollectionSpreadMsg)
                { key = config.key
                , today = config.today
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                maybeCollectionSpreadId
                |> Tuple.mapFirst EditCollectionSpread

        Route.NotFound hash ->
            ( NotFound hash, Cmd.none )


subscriptions : (Msg msg -> msg) -> Screen -> Sub msg
subscriptions lift screen =
    case screen of
        Overview overview ->
            Screen.Start.subscriptions (lift << OverviewMsg) overview

        TableOfContent tableOfContent ->
            Screen.TableOfContent.subscriptions (lift << TableOfContentMsg) tableOfContent

        DailySpread dailySpread ->
            Screen.DailySpread.subscriptions (lift << DailySpreadMsg) dailySpread

        MonthlySpread monthlySpread ->
            Screen.MonthlySpread.subscriptions (lift << MonthlySpreadMsg) monthlySpread

        CollectionSpread collectionSpread ->
            Screen.CollectionSpread.subscriptions (lift << CollectionSpreadMsg)
                collectionSpread

        EditBullet editBullet ->
            Screen.EditBullet.subscriptions (lift << EditBulletMsg) editBullet

        EditCollectionSpread editCollectionSpread ->
            Screen.EditCollectionSpread.subscriptions (lift << EditCollectionSpreadMsg)
                editCollectionSpread

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

        TableOfContentMsg msg_ ->
            case screen of
                TableOfContent tableOfContent ->
                    Screen.TableOfContent.update (lift << TableOfContentMsg)
                        { key = config.key
                        , today = config.today
                        , timeZone = config.timeZone
                        , parse = config.parse
                        , topAppBar = config.topAppBar
                        , fixedAdjust = config.fixedAdjust
                        }
                        msg_
                        tableOfContent
                        |> Tuple.mapFirst TableOfContent

                _ ->
                    ( screen, Cmd.none )

        CollectionSpreadMsg msg_ ->
            case screen of
                CollectionSpread collectionSpread ->
                    Screen.CollectionSpread.update (lift << CollectionSpreadMsg)
                        { key = config.key
                        , today = config.today
                        , parse = config.parse
                        , topAppBar = config.topAppBar
                        , fixedAdjust = config.fixedAdjust
                        }
                        msg_
                        collectionSpread
                        |> Tuple.mapFirst CollectionSpread

                _ ->
                    ( screen, Cmd.none )

        EditCollectionSpreadMsg msg_ ->
            case screen of
                EditCollectionSpread editCollectionSpread ->
                    Screen.EditCollectionSpread.update (lift << EditCollectionSpreadMsg)
                        { key = config.key
                        , today = config.today
                        , parse = config.parse
                        , topAppBar = config.topAppBar
                        , fixedAdjust = config.fixedAdjust
                        }
                        msg_
                        editCollectionSpread
                        |> Tuple.mapFirst EditCollectionSpread

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

        TableOfContent tableOfContent ->
            Screen.TableOfContent.view (lift << TableOfContentMsg)
                { key = config.key
                , today = config.today
                , timeZone = config.timeZone
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                tableOfContent

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

        CollectionSpread collectionSpread ->
            Screen.CollectionSpread.view (lift << CollectionSpreadMsg)
                { key = config.key
                , today = config.today
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                collectionSpread

        EditBullet editBullet ->
            Screen.EditBullet.view (lift << EditBulletMsg)
                { key = config.key
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                editBullet

        EditCollectionSpread editCollectionSpread ->
            Screen.EditCollectionSpread.view (lift << EditCollectionSpreadMsg)
                { key = config.key
                , today = config.today
                , parse = config.parse
                , topAppBar = config.topAppBar
                , fixedAdjust = config.fixedAdjust
                }
                editCollectionSpread

        NotFound notFound ->
            [ text <| "notFound: " ++ notFound ]
