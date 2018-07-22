module Main exposing (..)

import CollectionSpread
import DailySpread
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
import Material.List as Lists
import Material.Options as Options exposing (styled, cs, css, when)
import MonthlySpread
import Navigation
import Ports
import State exposing (State)
import Time.Calendar.Days as Calendar
import Time.Calendar.Gregorian as Calendar
import Url exposing (Url)


type alias Model =
    { mdc : Material.Model Msg
    , url : Url
    , monthlySpreads : Dict ( MonthlySpread.Year, MonthlySpread.Month ) MonthlySpread.Model
    , dailySpreads : Dict ( DailySpread.Year, DailySpread.Month, DailySpread.DayOfMonth ) DailySpread.Model
    , collectionSpreads : Dict CollectionSpread.Id CollectionSpread.Model
    , showNewSpreadDialog : Bool
    , today : Calendar.Day
    , now : Date
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , url = Url.Index
    , monthlySpreads = Dict.empty
    , dailySpreads = Dict.empty
    , collectionSpreads = Dict.empty
    , showNewSpreadDialog = False
    , today = Calendar.fromGregorian 1970 1 1
    , now = Date.fromTime 0
    }


type Msg
    = NoOp
    | Mdc (Material.Msg Msg)
    | SetUrl Url
    | MonthlySpreadMsg MonthlySpread.Year MonthlySpread.Month MonthlySpread.Msg
    | DailySpreadMsg DailySpread.Year DailySpread.Month DailySpread.DayOfMonth DailySpread.Msg
    | CollectionSpreadMsg CollectionSpread.Id CollectionSpread.Msg
    | NewSpreadClicked
    | NewSpreadDialogClosed
    | NewMonthlySpreadClicked
    | NewDailySpreadClicked
    | NewCollectionSpreadClicked
    | TodayChanged Calendar.Day
    | NowChanged Date
    | MonthlySpreadClicked MonthlySpread.Model
    | DailySpreadClicked DailySpread.Model
    | CollectionSpreadClicked CollectionSpread.Model


type alias Flags =
    { today : String
    , now : String
    , state : Maybe Value
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
        state =
            case
                flags.state
                    |> Maybe.map (Decode.decodeValue State.decode)
            of
                Just (Ok state) ->
                    Just state

                Just (Err err) ->
                    Debug.crash ("init: " ++ err)

                Nothing ->
                    Nothing

        monthlySpreads =
            state
                |> Maybe.map .monthlySpreads
                |> Maybe.map
                    (List.map
                        (\monthlySpread ->
                            ( ( monthlySpread.year, monthlySpread.month )
                            , { year = monthlySpread.year
                              , month = monthlySpread.month
                              , items =
                                    monthlySpread.items
                                        |> List.map
                                            (\item ->
                                                ( item.dayOfMonth, item.text )
                                            )
                                        |> Dict.fromList
                              , bullets = monthlySpread.bullets
                              }
                            )
                        )
                    )
                |> Maybe.map Dict.fromList
                |> Maybe.withDefault defaultModel.monthlySpreads

        dailySpreads =
            state
                |> Maybe.map .dailySpreads
                |> Maybe.map
                    (List.map
                        (\dailySpread ->
                            ( ( dailySpread.year
                              , dailySpread.month
                              , dailySpread.dayOfMonth
                              )
                            , { year = dailySpread.year
                              , month = dailySpread.month
                              , dayOfMonth = dailySpread.dayOfMonth
                              , bullets = dailySpread.bullets
                              }
                            )
                        )
                    )
                |> Maybe.map Dict.fromList
                |> Maybe.withDefault defaultModel.dailySpreads

        collectionSpreads =
            state
                |> Maybe.map .collectionSpreads
                |> Maybe.map
                    (List.map
                        (\collectionSpread ->
                            ( collectionSpread.id
                            , { id = collectionSpread.id
                              , createdDate = collectionSpread.createdDate
                              , title = collectionSpread.title
                              , bullets = collectionSpread.bullets
                              }
                            )
                        )
                    )
                |> Maybe.map Dict.fromList
                |> Maybe.withDefault defaultModel.collectionSpreads
    in
        ( { defaultModel
            | today = Ports.readDayUnsafe flags.today
            , now = Ports.readDateUnsafe flags.now
            , url = Debug.log "url" (Url.fromLocation location)
            , monthlySpreads = monthlySpreads
            , dailySpreads = dailySpreads
            , collectionSpreads = collectionSpreads
          }
        , Material.init Mdc
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdc model
        , Ports.today TodayChanged
        , Ports.now NowChanged
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    persist <|
        case msg of
            NoOp ->
                ( model, Cmd.none )

            Mdc msg_ ->
                Material.update Mdc msg_ model

            SetUrl url ->
                ( { model | url = url }, Cmd.none )

            MonthlySpreadMsg year month msg_ ->
                Dict.get ( year, month ) model.monthlySpreads
                    |> Maybe.withDefault (MonthlySpread.empty year month)
                    |> MonthlySpread.update (MonthlySpreadMsg year month) msg_
                    |> Tuple.mapFirst
                        (\monthlySpread ->
                            { model
                                | monthlySpreads =
                                    Dict.insert ( year, month )
                                        monthlySpread
                                        model.monthlySpreads
                            }
                        )

            DailySpreadMsg year month dayOfMonth msg_ ->
                Dict.get ( year, month, dayOfMonth ) model.dailySpreads
                    |> Maybe.withDefault (DailySpread.empty year month dayOfMonth)
                    |> DailySpread.update (DailySpreadMsg year month dayOfMonth) msg_
                    |> Tuple.mapFirst
                        (\dailySpread ->
                            { model
                                | dailySpreads =
                                    Dict.insert ( year, month, dayOfMonth )
                                        dailySpread
                                        model.dailySpreads
                            }
                        )

            CollectionSpreadMsg id msg_ ->
                Dict.get id model.collectionSpreads
                    |> Maybe.withDefault (CollectionSpread.empty id model.now)
                    |> CollectionSpread.update (CollectionSpreadMsg id) msg_
                    |> Tuple.mapFirst
                        (\collectionSpread ->
                            { model
                                | collectionSpreads =
                                    Dict.insert id
                                        collectionSpread
                                        model.collectionSpreads
                            }
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
                    if not (Dict.member ( year, month ) model.monthlySpreads) then
                        ( { model
                            | showNewSpreadDialog = False
                            , monthlySpreads =
                                Dict.insert ( year, month )
                                    newMonthlySpread
                                    model.monthlySpreads
                          }
                        , Navigation.newUrl (Url.toString (Url.MonthlySpread year month))
                        )
                    else
                        ( model, Cmd.none )

            NewDailySpreadClicked ->
                let
                    ( year, month, dayOfMonth ) =
                        Calendar.toGregorian model.today

                    newDailySpread =
                        DailySpread.empty year month dayOfMonth
                in
                    if not (Dict.member ( year, month, dayOfMonth ) model.dailySpreads) then
                        ( { model
                            | showNewSpreadDialog = False
                            , dailySpreads =
                                Dict.insert ( year, month, dayOfMonth )
                                    newDailySpread
                                    model.dailySpreads
                          }
                        , Navigation.newUrl
                            (Url.toString (Url.DailySpread year month dayOfMonth))
                        )
                    else
                        ( model, Cmd.none )

            NewCollectionSpreadClicked ->
                let
                    id =
                        toString (Date.toTime model.now)

                    createdDate =
                        model.now

                    newCollectionSpread =
                        CollectionSpread.empty id createdDate
                in
                    if not (Dict.member id model.collectionSpreads) then
                        ( { model
                            | showNewSpreadDialog = False
                            , collectionSpreads =
                                Dict.insert id
                                    newCollectionSpread
                                    model.collectionSpreads
                          }
                        , Navigation.newUrl (Url.toString (Url.CollectionSpread id))
                        )
                    else
                        ( model, Cmd.none )

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


persist : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
persist ( model, cmd ) =
    let
        monthlySpreads =
            model.monthlySpreads
                |> Dict.values
                |> List.map
                    (\monthlySpread ->
                        { year = monthlySpread.year
                        , month = monthlySpread.month
                        , items =
                            monthlySpread.items
                                |> Dict.toList
                                |> List.map
                                    (\( dayOfMonth, text ) ->
                                        { dayOfMonth = dayOfMonth, text = text }
                                    )
                        , bullets = monthlySpread.bullets
                        }
                    )

        dailySpreads =
            model.dailySpreads
                |> Dict.values
                |> List.map
                    (\dailySpread ->
                        { year = dailySpread.year
                        , month = dailySpread.month
                        , dayOfMonth = dailySpread.dayOfMonth
                        , bullets = dailySpread.bullets
                        }
                    )

        collectionSpreads =
            model.collectionSpreads
                |> Dict.values
                |> List.map
                    (\collectionSpread ->
                        { id = collectionSpread.id
                        , createdDate = collectionSpread.createdDate
                        , title = collectionSpread.title
                        , bullets = collectionSpread.bullets
                        }
                    )

        state =
            { monthlySpreads = monthlySpreads
            , dailySpreads = dailySpreads
            , collectionSpreads = collectionSpreads
            }
    in
        ( model, Cmd.batch [ Ports.persist state, cmd ] )


view : Model -> Html Msg
view model =
    case model.url of
        Url.Index ->
            viewIndex model

        Url.MonthlySpread year month ->
            viewMonthlySpread year month model

        Url.DailySpread year month dayOfMonth ->
            viewDailySpread year month dayOfMonth model

        Url.CollectionSpread id ->
            viewCollectionSpread id model

        Url.NotFound urlString ->
            viewNotFound urlString model


viewMonthlySpread : MonthlySpread.Year -> MonthlySpread.Month -> Model -> Html Msg
viewMonthlySpread year month model =
    MonthlySpread.view (MonthlySpreadMsg year month)
        (model.monthlySpreads
            |> Dict.get ( year, month )
            |> Maybe.withDefault (MonthlySpread.empty year month)
        )


viewDailySpread :
    DailySpread.Year
    -> DailySpread.Month
    -> DailySpread.DayOfMonth
    -> Model
    -> Html Msg
viewDailySpread year month dayOfMonth model =
    DailySpread.view (DailySpreadMsg year month dayOfMonth)
        (model.dailySpreads
            |> Dict.get ( year, month, dayOfMonth )
            |> Maybe.withDefault (DailySpread.empty year month dayOfMonth)
        )


viewCollectionSpread : CollectionSpread.Id -> Model -> Html Msg
viewCollectionSpread id model =
    CollectionSpread.view (CollectionSpreadMsg id)
        (model.collectionSpreads
            |> Dict.get id
            |> Maybe.withDefault (CollectionSpread.empty id model.now)
        )


viewNotFound : String -> Model -> Html Msg
viewNotFound urlString model =
    Html.div
        [ Html.class "not-found"
        ]
        [ text ("URL not found: " ++ urlString)
        ]


viewIndex : Model -> Html Msg
viewIndex model =
    Html.div
        []
        [ index model
        , newSpreadDialog model
        ]


index : Model -> Html Msg
index model =
    Html.div
        [ Html.class "index"
        ]
        [ Html.div
            [ Html.class "index__title"
            ]
            [ Html.h1 []
                [ text "Index" ]
            ]
        , Html.ol
            [ Html.class "index__wrapper"
            ]
            (List.concat
                [ List.map
                    (\monthlySpread ->
                        Html.li
                            [ Html.onClick (MonthlySpreadClicked monthlySpread)
                            ]
                            [ text (MonthlySpread.title monthlySpread)
                            ]
                    )
                    (Dict.values model.monthlySpreads)
                , List.map
                    (\dailySpread ->
                        Html.li
                            [ Html.onClick (DailySpreadClicked dailySpread)
                            ]
                            [ text (DailySpread.title dailySpread)
                            ]
                    )
                    (Dict.values model.dailySpreads)
                , List.map
                    (\collectionSpread ->
                        Html.li
                            [ Html.onClick (CollectionSpreadClicked collectionSpread)
                            ]
                            [ text (CollectionSpread.title collectionSpread)
                            ]
                    )
                    (Dict.values model.collectionSpreads)
                ]
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
                    []
                    [ Lists.li
                        [ Options.onClick NewMonthlySpreadClicked
                        , Options.attribute (Html.tabindex 0)
                        ]
                        [ Lists.text []
                            [ text "New monthly spread"
                            ]
                        ]
                    , Lists.li
                        [ Options.onClick NewDailySpreadClicked
                        , Options.attribute (Html.tabindex 0)
                        ]
                        [ Lists.text []
                            [ text "New daily spread"
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
