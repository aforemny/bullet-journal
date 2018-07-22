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
import Ports
import State exposing (State)
import Time.Calendar.Days as Calendar
import Time.Calendar.Gregorian as Calendar
import Time.Format.Locale as Calendar
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
import Type.DailySpread as DailySpread exposing (DailySpread)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import Url exposing (Url)
import View
import View.CollectionSpread
import View.DailySpread
import View.MonthlySpread


type alias Model =
    { mdc : Material.Model Msg
    , url : Url
    , monthlySpread : View.MonthlySpread.Model Msg
    , dailySpread : View.DailySpread.Model Msg
    , collectionSpread : View.CollectionSpread.Model Msg
    , showNewSpreadDialog : Bool
    , today : Calendar.Day
    , now : Date
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , url = Url.Index
    , monthlySpread = View.MonthlySpread.defaultModel
    , dailySpread = View.DailySpread.defaultModel
    , collectionSpread = View.CollectionSpread.defaultModel
    , showNewSpreadDialog = False
    , today = Calendar.fromGregorian 1970 1 1
    , now = Date.fromTime 0
    }


type Msg
    = NoOp
    | Mdc (Material.Msg Msg)
    | SetUrl Url
    | CollectionSpreadMsg (View.CollectionSpread.Msg Msg)
    | DailySpreadMsg (View.DailySpread.Msg Msg)
    | MonthlySpreadMsg (View.MonthlySpread.Msg Msg)
    | NewSpreadClicked
    | NewSpreadDialogClosed
    | NewMonthlySpreadClicked
    | NewDailySpreadClicked
    | NewCollectionSpreadClicked
    | TodayChanged Calendar.Day
    | NowChanged Date
    | MonthlySpreadClicked MonthlySpread
    | DailySpreadClicked DailySpread
    | CollectionSpreadClicked CollectionSpread
    | BackClicked


type alias Flags =
    { today : String
    , now :
        String
        -- , state : Maybe Value
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
    --    let
    --        state =
    --            case
    --                flags.state
    --                    |> Maybe.map (Decode.decodeValue State.decode)
    --            of
    --                Just (Ok state) ->
    --                    Just state
    --
    --                Just (Err err) ->
    --                    Debug.crash ("init: " ++ err)
    --
    --                Nothing ->
    --                    Nothing
    --
    --        monthlySpreads =
    --            state
    --                |> Maybe.map .monthlySpreads
    --                |> Maybe.map
    --                    (List.map
    --                        (\monthlySpread ->
    --                            ( ( monthlySpread.year, monthlySpread.month )
    --                            , { year = monthlySpread.year
    --                              , month = monthlySpread.month
    --                              , items =
    --                                    monthlySpread.items
    --                                        |> List.map
    --                                            (\item ->
    --                                                ( item.dayOfMonth, item.text )
    --                                            )
    --                                        |> Dict.fromList
    --                              , bullets = monthlySpread.bullets
    --                              }
    --                            )
    --                        )
    --                    )
    --                |> Maybe.map Dict.fromList
    --                |> Maybe.withDefault defaultModel.monthlySpreads
    --
    --        dailySpreads =
    --            state
    --                |> Maybe.map .dailySpreads
    --                |> Maybe.map
    --                    (List.map
    --                        (\dailySpread ->
    --                            ( ( dailySpread.year
    --                              , dailySpread.month
    --                              , dailySpread.dayOfMonth
    --                              )
    --                            , { year = dailySpread.year
    --                              , month = dailySpread.month
    --                              , dayOfMonth = dailySpread.dayOfMonth
    --                              , bullets = dailySpread.bullets
    --                              }
    --                            )
    --                        )
    --                    )
    --                |> Maybe.map Dict.fromList
    --                |> Maybe.withDefault defaultModel.dailySpreads
    --
    --        collectionSpreads =
    --            state
    --                |> Maybe.map .collectionSpreads
    --                |> Maybe.map
    --                    (List.map
    --                        (\collectionSpread ->
    --                            ( collectionSpread.id
    --                            , { id = collectionSpread.id
    --                              , createdDate = collectionSpread.createdDate
    --                              , title = collectionSpread.title
    --                              , bullets = collectionSpread.bullets
    --                              }
    --                            )
    --                        )
    --                    )
    --                |> Maybe.map Dict.fromList
    --                |> Maybe.withDefault defaultModel.collectionSpreads
    --    in
    ( { defaultModel
        | today = Ports.readDayUnsafe flags.today
        , now = Ports.readDateUnsafe flags.now
        , url =
            Url.fromLocation location
            --            , monthlySpreads = monthlySpreads
            --            , dailySpreads = dailySpreads
            --            , collectionSpreads = collectionSpreads
      }
    , Cmd.batch
        [ Material.init Mdc
        , View.CollectionSpread.init CollectionSpreadMsg
        , View.DailySpread.init DailySpreadMsg
        , View.MonthlySpread.init MonthlySpreadMsg
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdc model
        , Ports.today TodayChanged
        , Ports.now NowChanged
        , View.CollectionSpread.subscriptions CollectionSpreadMsg model.collectionSpread
        , View.DailySpread.subscriptions DailySpreadMsg model.dailySpread
        , View.MonthlySpread.subscriptions MonthlySpreadMsg model.monthlySpread
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- persist <|
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Mdc msg_ ->
            Material.update Mdc msg_ model

        SetUrl url ->
            ( { model | url = url }, Cmd.none )

        MonthlySpreadMsg msg_ ->
            model.monthlySpread
                |> View.MonthlySpread.update MonthlySpreadMsg msg_
                |> Tuple.mapFirst
                    (\monthlySpread ->
                        { model | monthlySpread = monthlySpread }
                    )

        DailySpreadMsg msg_ ->
            model.dailySpread
                |> View.DailySpread.update DailySpreadMsg msg_
                |> Tuple.mapFirst
                    (\dailySpread ->
                        { model | dailySpread = dailySpread }
                    )

        CollectionSpreadMsg msg_ ->
            model.collectionSpread
                |> View.CollectionSpread.update CollectionSpreadMsg msg_
                |> Tuple.mapFirst
                    (\collectionSpread ->
                        { model | collectionSpread = collectionSpread }
                    )

        NewSpreadClicked ->
            ( { model | showNewSpreadDialog = True }, Cmd.none )

        NewSpreadDialogClosed ->
            ( { model | showNewSpreadDialog = False }, Cmd.none )

        NewMonthlySpreadClicked ->
            let
                ( year, month, _ ) =
                    Calendar.toGregorian model.today

                newMonthlySpread =
                    MonthlySpread.empty year month
            in
                ( { model
                    | showNewSpreadDialog =
                        False
                        --                    , monthlySpreads =
                        --                        if not (Dict.member ( year, month ) model.monthlySpreads) then
                        --                            Dict.insert ( year, month )
                        --                                newMonthlySpread
                        --                                model.monthlySpreads
                        --                        else
                        --                            model.monthlySpreads
                  }
                , Navigation.newUrl (Url.toString (Url.MonthlySpread year month))
                )

        NewDailySpreadClicked ->
            let
                ( year, month, dayOfMonth ) =
                    Calendar.toGregorian model.today

                newDailySpread =
                    DailySpread.empty year month dayOfMonth
            in
                ( { model
                    | showNewSpreadDialog =
                        False
                        --                    , dailySpreads =
                        --                        if not (Dict.member ( year, month, dayOfMonth ) model.dailySpreads) then
                        --                            Dict.insert ( year, month, dayOfMonth )
                        --                                newDailySpread
                        --                                model.dailySpreads
                        --                        else
                        --                            model.dailySpreads
                  }
                , Navigation.newUrl
                    (Url.toString (Url.DailySpread year month dayOfMonth))
                )

        NewCollectionSpreadClicked ->
            let
                id =
                    toString (Date.toTime model.now)

                createdDate =
                    model.now

                newCollectionSpread =
                    CollectionSpread.empty id createdDate
            in
                --                if not (Dict.member id model.collectionSpreads) then
                ( { model
                    | showNewSpreadDialog =
                        False
                        --                        , collectionSpreads =
                        --                            Dict.insert id
                        --                                newCollectionSpread
                        --                                model.collectionSpreads
                  }
                , Navigation.newUrl (Url.toString (Url.CollectionSpread id))
                )

        --                else
        --                    ( model, Cmd.none )
        TodayChanged today ->
            ( { model | today = today }, Cmd.none )

        NowChanged now ->
            ( { model | now = now }, Cmd.none )

        MonthlySpreadClicked monthlySpread ->
            ( model
            , Navigation.newUrl
                (Url.toString
                    (Url.MonthlySpread monthlySpread.year monthlySpread.month)
                )
            )

        DailySpreadClicked dailySpread ->
            ( model
            , Navigation.newUrl
                (Url.toString
                    (Url.DailySpread dailySpread.year
                        dailySpread.month
                        dailySpread.dayOfMonth
                    )
                )
            )

        CollectionSpreadClicked collectionSpread ->
            ( model
            , Navigation.newUrl (Url.toString (Url.CollectionSpread collectionSpread.id))
            )

        BackClicked ->
            ( model
            , Navigation.newUrl (Url.toString Url.Index)
            )



--
--
--persist : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
--persist ( model, cmd ) =
--    let
--        monthlySpreads =
--            model.monthlySpreads
--                |> Dict.values
--                |> List.map
--                    (\monthlySpread ->
--                        { year = monthlySpread.year
--                        , month = monthlySpread.month
--                        , items =
--                            monthlySpread.items
--                                |> Dict.toList
--                                |> List.map
--                                    (\( dayOfMonth, text ) ->
--                                        { dayOfMonth = dayOfMonth, text = text }
--                                    )
--                        , bullets = monthlySpread.bullets
--                        }
--                    )
--
--        dailySpreads =
--            model.dailySpreads
--                |> Dict.values
--                |> List.map
--                    (\dailySpread ->
--                        { year = dailySpread.year
--                        , month = dailySpread.month
--                        , dayOfMonth = dailySpread.dayOfMonth
--                        , bullets = dailySpread.bullets
--                        }
--                    )
--
--        collectionSpreads =
--            model.collectionSpreads
--                |> Dict.values
--                |> List.map
--                    (\collectionSpread ->
--                        { id = collectionSpread.id
--                        , createdDate = collectionSpread.createdDate
--                        , title = collectionSpread.title
--                        , bullets = collectionSpread.bullets
--                        }
--                    )
--
--        state =
--            { monthlySpreads = monthlySpreads
--            , dailySpreads = dailySpreads
--            , collectionSpreads = collectionSpreads
--            }
--    in
--        ( model, Cmd.batch [ Ports.persist state, cmd ] )


view : Model -> Html Msg
view model =
    let
        viewConfig =
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
            }
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

                    Url.MonthlySpread year month ->
                        viewMonthlySpread viewConfig year month model

                    Url.DailySpread year month dayOfMonth ->
                        viewDailySpread viewConfig year month dayOfMonth model

                    Url.CollectionSpread id ->
                        viewCollectionSpread viewConfig id model

                    Url.NotFound urlString ->
                        viewNotFound viewConfig urlString model
                ]
            ]


viewMonthlySpread :
    View.Config Msg
    -> MonthlySpread.Year
    -> MonthlySpread.Month
    -> Model
    -> Html Msg
viewMonthlySpread viewConfig year month model =
    View.MonthlySpread.view MonthlySpreadMsg
        viewConfig
        model.monthlySpread


viewDailySpread :
    View.Config Msg
    -> DailySpread.Year
    -> DailySpread.Month
    -> DailySpread.DayOfMonth
    -> Model
    -> Html Msg
viewDailySpread viewConfig year month dayOfMonth model =
    View.DailySpread.view DailySpreadMsg
        viewConfig
        model.dailySpread


viewCollectionSpread : View.Config Msg -> CollectionSpread.Id -> Model -> Html Msg
viewCollectionSpread viewConfig id model =
    View.CollectionSpread.view CollectionSpreadMsg
        viewConfig
        model.collectionSpread


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
    Html.div
        []
        [ viewConfig.toolbar
            { additionalSections = []
            }
        , index model
        , newSpreadDialog model
        ]


index : Model -> Html Msg
index model =
    --    let
    --        monthlySpreads =
    --            List.map
    --                (\monthlySpread ->
    --                    ( MonthlySpread.canonicalDate monthlySpread
    --                    , Lists.li
    --                        [ Options.onClick (MonthlySpreadClicked monthlySpread)
    --                        ]
    --                        [ text (MonthlySpread.title monthlySpread)
    --                        ]
    --                    )
    --                )
    --                (Dict.values model.monthlySpreads)
    --
    --        dailySpreads =
    --            List.map
    --                (\dailySpread ->
    --                    ( DailySpread.canonicalDate dailySpread
    --                    , Lists.li
    --                        [ Options.onClick (DailySpreadClicked dailySpread)
    --                        ]
    --                        [ text (DailySpread.title dailySpread)
    --                        ]
    --                    )
    --                )
    --                (Dict.values model.dailySpreads)
    --
    --        collectionSpreads =
    --            List.map
    --                (\collectionSpread ->
    --                    ( CollectionSpread.canonicalDate collectionSpread
    --                    , Lists.li
    --                        [ Options.onClick (CollectionSpreadClicked collectionSpread)
    --                        ]
    --                        [ text (CollectionSpread.title collectionSpread)
    --                        ]
    --                    )
    --                )
    --                (Dict.values model.collectionSpreads)
    --    in
    Html.div
        [ Html.class "index"
        ]
        [ Html.div
            [ Html.class "index__title"
            ]
            [ Html.h1 []
                [ text "Index" ]
            ]
        , Lists.ol
            []
            (List.map Tuple.second <|
                List.sortBy Tuple.first <|
                    []
             --                        List.concat
             --                            [ monthlySpreads
             --                            , dailySpreads
             --                            , collectionSpreads
             --                            ]
            )
        , Fab.view Mdc
            "index__new-spread"
            model.mdc
            [ cs "index__new-spread"
            , Fab.ripple
            , Options.onClick NewSpreadClicked
            ]
            "add"
        ]


newSpreadDialog : Model -> Html Msg
newSpreadDialog model =
    let
        ( year, month, dayOfMonth ) =
            Calendar.toGregorian model.today

        monthName =
            case Calendar.defaultTimeLocale of
                Calendar.TimeLocale { months } ->
                    List.head (List.drop (month - 1) months)
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault ""

        monthlySpreadName =
            monthName ++ " " ++ toString year

        dailySpreadName =
            String.join " "
                [ toString dayOfMonth
                , monthName
                , toString year
                ]
    in
        Dialog.view Mdc
            "new-spread-dialog"
            model.mdc
            [ Dialog.onClose NewSpreadDialogClosed
            , when model.showNewSpreadDialog Dialog.open
            ]
            [ Dialog.surface []
                [ Dialog.header []
                    []
                , Dialog.body []
                    [ Lists.ul
                        [ Lists.twoLine
                        ]
                        [ Lists.li
                            [ Options.onClick NewMonthlySpreadClicked
                            , Options.attribute (Html.tabindex 0)
                            ]
                            [ Lists.text []
                                [ text "New monthly spread"
                                , Lists.secondaryText []
                                    [ text monthlySpreadName
                                    ]
                                ]
                            ]
                        , Lists.li
                            [ Options.onClick NewDailySpreadClicked
                            , Options.attribute (Html.tabindex 0)
                            ]
                            [ Lists.text []
                                [ text "New daily spread"
                                , Lists.secondaryText []
                                    [ text dailySpreadName
                                    ]
                                ]
                            ]
                        , Lists.li
                            [ Options.onClick NewCollectionSpreadClicked
                            , Options.attribute (Html.tabindex 0)
                            ]
                            [ Lists.text []
                                [ text "New collection"
                                ]
                            ]
                        ]
                    ]
                , Dialog.footer []
                    [ Button.view Mdc
                        "new-spread-dialog__cancel"
                        model.mdc
                        [ Button.ripple
                        , Button.onClick NewSpreadDialogClosed
                        , Dialog.cancel
                        ]
                        [ text "Cancel"
                        ]
                    ]
                ]
            ]
