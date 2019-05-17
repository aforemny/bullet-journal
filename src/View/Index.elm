module View.Index exposing (Model, Msg(..), defaultModel, init, newSpreadDialog, subscriptions, update, view)

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Material.Button exposing (buttonConfig, textButton)
import Material.Card exposing (card, cardBlock, cardConfig)
import Material.Dialog exposing (cancelButton, dialog, dialogConfig)
import Material.Fab exposing (fab, fabConfig)
import Material.Icon exposing (icon, iconConfig)
import Material.List exposing (list, listConfig, listItem, listItemConfig, listItemPrimaryText, listItemSecondaryText, listItemText)
import Material.TopAppBar as TopAppBar
import Parse
import Route
import Task exposing (Task)
import Time
import Time.Calendar.Gregorian as Calendar
import Time.Format.Locale as Calendar
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
import Type.DailySpread as DailySpread exposing (DailySpread)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import View


type alias Model =
    { monthlySpreads : List (Parse.Object MonthlySpread)
    , dailySpreads : List (Parse.Object DailySpread)
    , collectionSpreads : List (Parse.Object CollectionSpread)
    , error : Maybe Parse.Error
    , showNewSpreadDialog : Bool
    }


defaultModel : Model
defaultModel =
    { monthlySpreads = []
    , dailySpreads = []
    , collectionSpreads = []
    , error = Nothing
    , showNewSpreadDialog = False
    }


type Msg msg
    = MonthlySpreadsResult (Result Parse.Error (List (Parse.Object MonthlySpread)))
    | DailySpreadsResult (Result Parse.Error (List (Parse.Object DailySpread)))
    | CollectionSpreadsResult (Result Parse.Error (List (Parse.Object CollectionSpread)))
    | NewSpreadClicked
    | MonthlySpreadClicked (Parse.Object MonthlySpread)
    | DailySpreadClicked (Parse.Object DailySpread)
    | CollectionSpreadClicked (Parse.Object CollectionSpread)
    | NewSpreadDialogClosed
    | NewMonthlySpreadClicked
    | NewDailySpreadClicked
    | NewCollectionSpreadClicked
    | NewMonthlySpreadClickedResult (Result Parse.Error (Parse.ObjectId MonthlySpread))
    | NewDailySpreadClickedResult (Result Parse.Error (Parse.ObjectId DailySpread))
    | NewCollectionSpreadClickedResult (Result Parse.Error (Parse.ObjectId CollectionSpread))


init : (Msg msg -> msg) -> View.Config msg -> Model -> ( Model, Cmd msg )
init lift viewConfig model =
    ( { defaultModel
        | monthlySpreads = model.monthlySpreads
        , dailySpreads = model.dailySpreads
        , collectionSpreads = model.collectionSpreads
      }
    , Cmd.batch
        [ Parse.send viewConfig.parse
            (lift << MonthlySpreadsResult)
            (Parse.query MonthlySpread.decode (Parse.emptyQuery "MonthlySpread"))
        , Parse.send viewConfig.parse
            (lift << DailySpreadsResult)
            (Parse.query DailySpread.decode (Parse.emptyQuery "DailySpread"))
        , Parse.send viewConfig.parse
            (lift << CollectionSpreadsResult)
            (Parse.query CollectionSpread.decode (Parse.emptyQuery "CollectionSpread"))
        ]
    )


subscriptions : (Msg msg -> msg) -> Model -> Sub msg
subscriptions lift model =
    Sub.none


update :
    (Msg msg -> msg)
    -> View.Config msg
    -> Msg msg
    -> Model
    -> ( Model, Cmd msg )
update lift viewConfig msg model =
    case msg of
        MonthlySpreadsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        MonthlySpreadsResult (Ok monthlySpreads) ->
            ( { model | monthlySpreads = monthlySpreads }, Cmd.none )

        DailySpreadsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DailySpreadsResult (Ok dailySpreads) ->
            ( { model | dailySpreads = dailySpreads }, Cmd.none )

        CollectionSpreadsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        CollectionSpreadsResult (Ok collectionSpreads) ->
            ( { model | collectionSpreads = collectionSpreads }, Cmd.none )

        NewSpreadClicked ->
            ( { model | showNewSpreadDialog = True }, Cmd.none )

        NewSpreadDialogClosed ->
            ( { model | showNewSpreadDialog = False }, Cmd.none )

        MonthlySpreadClicked monthlySpread ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.MonthlySpread monthlySpread.objectId))
            )

        DailySpreadClicked dailySpread ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.DailySpread dailySpread.objectId))
            )

        CollectionSpreadClicked collectionSpread ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.CollectionSpread collectionSpread.objectId))
            )

        NewMonthlySpreadClicked ->
            let
                ( year, month, _ ) =
                    Calendar.toGregorian viewConfig.today

                monthlySpread =
                    MonthlySpread.empty year month
            in
            ( model
            , Task.attempt (lift << NewMonthlySpreadClickedResult)
                (MonthlySpread.create viewConfig.parse monthlySpread)
            )

        NewMonthlySpreadClickedResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        NewMonthlySpreadClickedResult (Ok objectId) ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.MonthlySpread objectId))
            )

        NewDailySpreadClicked ->
            let
                ( year, month, dayOfMonth ) =
                    Calendar.toGregorian viewConfig.today

                dailySpread =
                    DailySpread.empty year month dayOfMonth
            in
            ( model
            , Task.attempt (lift << NewDailySpreadClickedResult)
                (DailySpread.create viewConfig.parse dailySpread)
            )

        NewDailySpreadClickedResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        NewDailySpreadClickedResult (Ok objectId) ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.DailySpread objectId))
            )

        NewCollectionSpreadClicked ->
            let
                collectionSpread =
                    CollectionSpread.empty "New collection"
            in
            ( model
            , Task.attempt (lift << NewCollectionSpreadClickedResult)
                (CollectionSpread.create viewConfig.parse collectionSpread)
            )

        NewCollectionSpreadClickedResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        NewCollectionSpreadClickedResult (Ok objectId) ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.CollectionSpread objectId))
            )


view : (Msg msg -> msg) -> View.Config msg -> Model -> Html msg
view lift viewConfig model =
    let
        monthlySpreads =
            List.map
                (\monthlySpread_ ->
                    let
                        monthlySpread =
                            MonthlySpread.fromParseObject monthlySpread_
                    in
                    ( MonthlySpread.canonicalDate monthlySpread
                    , listItem
                        { listItemConfig
                            | additionalAttributes =
                                [ Html.Events.onClick
                                    (lift (MonthlySpreadClicked monthlySpread_))
                                ]
                        }
                        [ text (MonthlySpread.title monthlySpread) ]
                    )
                )
                model.monthlySpreads

        dailySpreads =
            List.map
                (\dailySpread_ ->
                    let
                        dailySpread =
                            DailySpread.fromParseObject dailySpread_
                    in
                    ( DailySpread.canonicalDate dailySpread
                    , listItem
                        { listItemConfig
                            | additionalAttributes =
                                [ Html.Events.onClick
                                    (lift (DailySpreadClicked dailySpread_))
                                ]
                        }
                        [ text (DailySpread.title dailySpread) ]
                    )
                )
                model.dailySpreads

        collectionSpreads =
            List.map
                (\collectionSpread_ ->
                    let
                        collectionSpread =
                            CollectionSpread.fromParseObject collectionSpread_
                    in
                    ( CollectionSpread.canonicalDate viewConfig.timeZone collectionSpread
                    , listItem
                        { listItemConfig
                            | additionalAttributes =
                                [ Html.Events.onClick
                                    (lift (CollectionSpreadClicked collectionSpread_))
                                ]
                        }
                        [ text (CollectionSpread.title collectionSpread) ]
                    )
                )
                model.collectionSpreads
    in
    Html.div
        [ class "index"
        ]
        [ viewConfig.toolbar
            { title = "Index"
            , menuIcon =
                icon { iconConfig | additionalAttributes = [ TopAppBar.navigationIcon ] }
                    "menu"
            , additionalSections = []
            }
        , card
            { cardConfig
                | additionalAttributes =
                    [ class "index__wrapper" ]
            }
            { blocks =
                [ cardBlock <|
                    Html.div []
                        [ Html.div
                            [ class "index__primary" ]
                            [ Html.h2
                                [ class "index__title" ]
                                [ text "Index" ]
                            , Html.h3
                                [ class "index__subtitle" ]
                                [ text "Collections will show here, so you can quickly find them." ]
                            ]
                        , list
                            { listConfig
                                | additionalAttributes =
                                    [ class "index__items-wrapper" ]
                            }
                            (List.map Tuple.second <|
                                List.sortBy Tuple.first <|
                                    List.concat
                                        [ monthlySpreads
                                        , dailySpreads
                                        , collectionSpreads
                                        ]
                            )
                        , fab
                            { fabConfig
                                | onClick = Just (lift NewSpreadClicked)
                                , additionalAttributes = [ class "index__new-spread" ]
                            }
                            "add"
                        ]
                ]
            , actions = Nothing
            }
        , newSpreadDialog lift viewConfig model
        ]


newSpreadDialog : (Msg msg -> msg) -> View.Config msg -> Model -> Html msg
newSpreadDialog lift viewConfig model =
    let
        ( year, month, dayOfMonth ) =
            Calendar.toGregorian viewConfig.today

        monthName =
            case Calendar.defaultTimeLocale of
                Calendar.TimeLocale { months } ->
                    List.head (List.drop (month - 1) months)
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault ""

        monthlySpreadName =
            monthName ++ " " ++ String.fromInt year

        dailySpreadName =
            String.join " "
                [ String.fromInt dayOfMonth
                , monthName
                , String.fromInt year
                ]
    in
    dialog
        { dialogConfig
            | onClose = Just (lift NewSpreadDialogClosed)
            , open = model.showNewSpreadDialog
        }
        { title = Nothing
        , content =
            [ list
                { listConfig | twoLine = True }
                [ listItem
                    { listItemConfig
                        | additionalAttributes =
                            [ Html.Events.onClick (lift NewMonthlySpreadClicked)
                            , Html.Attributes.tabindex 0
                            ]
                    }
                    [ listItemText []
                        [ text "New monthly spread"
                        , listItemSecondaryText [] [ text monthlySpreadName ]
                        ]
                    ]
                , listItem
                    { listItemConfig
                        | additionalAttributes =
                            [ Html.Events.onClick (lift NewDailySpreadClicked)
                            , Html.Attributes.tabindex 0
                            ]
                    }
                    [ listItemText []
                        [ text "New daily spread"
                        , listItemSecondaryText [] [ text dailySpreadName ]
                        ]
                    ]
                , listItem
                    { listItemConfig
                        | additionalAttributes =
                            [ Html.Events.onClick (lift NewCollectionSpreadClicked)
                            , Html.Attributes.tabindex 0
                            ]
                    }
                    [ listItemText [] [ text "New collection" ] ]
                ]
            ]
        , actions =
            [ cancelButton
                { buttonConfig
                    | onClick = Just (lift NewSpreadDialogClosed)
                }
                "Cancel"
            ]
        }
