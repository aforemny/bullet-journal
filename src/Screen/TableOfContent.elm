module Screen.TableOfContent exposing
    ( Config
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Material.Button exposing (buttonConfig, textButton)
import Material.Card exposing (card, cardBlock, cardConfig)
import Material.Dialog exposing (dialog, dialogConfig)
import Material.Fab exposing (fab, fabConfig)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.List exposing (list, listConfig, listItem, listItemConfig, listItemPrimaryText, listItemSecondaryText, listItemText)
import Material.TopAppBar as TopAppBar
import Parse
import Route
import Task exposing (Task)
import Time
import Time.Calendar.Days as Calendar
import Time.Calendar.Gregorian as Calendar
import Time.Format.Locale as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)


type alias Config msg =
    { key : Browser.Navigation.Key
    , parse : Parse.Config
    , today : Calendar.Day
    , timeZone : Time.Zone
    , topAppBar :
        { title : String
        , menuIcon : Maybe (Html msg)
        , additionalSections : List (Html msg)
        }
        -> Html msg
    , fixedAdjust : Html.Attribute msg
    }


type alias Model =
    { collectionSpreads : List (Parse.Object CollectionSpread)
    , bullets : List (Parse.Object Bullet)
    , error : Maybe Parse.Error
    , showNewSpreadDialog : Bool
    }


defaultModel : Model
defaultModel =
    { collectionSpreads = []
    , bullets = []
    , error = Nothing
    , showNewSpreadDialog = False
    }


type Msg msg
    = CollectionSpreadsResult (Result Parse.Error (List (Parse.Object CollectionSpread)))
    | NewSpreadClicked
      -- | MonthlySpreadClicked (Parse.Object MonthlySpread)
      -- | DailySpreadClicked (Parse.Object DailySpread)
    | CollectionSpreadClicked (Parse.Object CollectionSpread)
    | NewSpreadDialogClosed
      -- | NewMonthlySpreadClicked
      -- | NewDailySpreadClicked
    | NewCollectionSpreadClicked
      -- | NewMonthlySpreadClickedResult (Result Parse.Error (Parse.ObjectId MonthlySpread))
      -- | NewDailySpreadClickedResult (Result Parse.Error (Parse.ObjectId DailySpread))
    | NewCollectionSpreadClickedResult (Result Parse.Error (Parse.ObjectId CollectionSpread))
    | BulletsResult (Result Parse.Error (List (Parse.Object Bullet)))


init : (Msg msg -> msg) -> Config msg -> ( Model, Cmd msg )
init lift config =
    ( defaultModel
    , Cmd.batch
        [ Parse.send config.parse
            (lift << CollectionSpreadsResult)
            (Parse.query CollectionSpread.decode (Parse.emptyQuery "CollectionSpread"))
        , Parse.send config.parse
            (lift << BulletsResult)
            (Parse.query Bullet.decode (Parse.emptyQuery "Bullet"))
        ]
    )


subscriptions : (Msg msg -> msg) -> Model -> Sub msg
subscriptions lift model =
    Sub.none


update :
    (Msg msg -> msg)
    -> Config msg
    -> Msg msg
    -> Model
    -> ( Model, Cmd msg )
update lift config msg model =
    case msg of
        --        MonthlySpreadsResult (Err err) ->
        --            ( { model | error = Just err }, Cmd.none )
        --
        --        MonthlySpreadsResult (Ok monthlySpreads) ->
        --            ( { model | monthlySpreads = monthlySpreads }, Cmd.none )
        --        DailySpreadsResult (Err err) ->
        --            ( { model | error = Just err }, Cmd.none )
        --        DailySpreadsResult (Ok dailySpreads) ->
        --            ( { model | dailySpreads = dailySpreads }, Cmd.none )
        CollectionSpreadsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        CollectionSpreadsResult (Ok collectionSpreads) ->
            ( { model | collectionSpreads = collectionSpreads }, Cmd.none )

        NewSpreadClicked ->
            ( { model | showNewSpreadDialog = True }, Cmd.none )

        NewSpreadDialogClosed ->
            ( { model | showNewSpreadDialog = False }, Cmd.none )

        --        MonthlySpreadClicked monthlySpread ->
        --            ( model
        --            , Browser.Navigation.pushUrl config.key
        --                (Route.toString (Route.MonthlySpread monthlySpread.objectId))
        --            )
        --        DailySpreadClicked dailySpread ->
        --            ( model
        --            , Browser.Navigation.pushUrl config.key
        --                (Route.toString (Route.DailySpread dailySpread.objectId))
        --            )
        CollectionSpreadClicked collectionSpread ->
            ( model
            , Browser.Navigation.pushUrl config.key
                (Route.toString (Route.CollectionSpread collectionSpread.objectId))
            )

        --        NewMonthlySpreadClicked ->
        --            let
        --                ( year, month, _ ) =
        --                    Calendar.toGregorian config.today
        --
        --                monthlySpread =
        --                    MonthlySpread.empty year month
        --            in
        --            ( model
        --            , Task.attempt (lift << NewMonthlySpreadClickedResult)
        --                (MonthlySpread.create config.parse monthlySpread)
        --            )
        --        NewMonthlySpreadClickedResult (Err err) ->
        --            ( { model | error = Just err }, Cmd.none )
        --
        --        NewMonthlySpreadClickedResult (Ok objectId) ->
        --            ( model
        --            , Browser.Navigation.pushUrl config.key
        --                (Route.toString (Route.MonthlySpread objectId))
        --            )
        --        NewDailySpreadClicked ->
        --            let
        --                ( year, month, dayOfMonth ) =
        --                    Calendar.toGregorian config.today
        --
        --                dailySpread =
        --                    DailySpread.empty year month dayOfMonth
        --            in
        --            ( model
        --            , Task.attempt (lift << NewDailySpreadClickedResult)
        --                (DailySpread.create config.parse dailySpread)
        --            )
        --        NewDailySpreadClickedResult (Err err) ->
        --            ( { model | error = Just err }, Cmd.none )
        --        NewDailySpreadClickedResult (Ok objectId) ->
        --            ( model
        --            , Browser.Navigation.pushUrl config.key
        --                (Route.toString (Route.DailySpread objectId))
        --            ))
        NewCollectionSpreadClicked ->
            let
                collectionSpread =
                    CollectionSpread.empty "New collection"
            in
            ( model
            , Task.attempt (lift << NewCollectionSpreadClickedResult)
                (CollectionSpread.create config.parse collectionSpread)
            )

        NewCollectionSpreadClickedResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        NewCollectionSpreadClickedResult (Ok objectId) ->
            ( model
            , Browser.Navigation.pushUrl config.key
                (Route.toString (Route.CollectionSpread objectId))
            )

        BulletsResult (Err _) ->
            ( model, Cmd.none )

        BulletsResult (Ok bullets) ->
            ( { model | bullets = bullets }, Cmd.none )


view : (Msg msg -> msg) -> Config msg -> Model -> List (Html msg)
view lift config model =
    let
        --        monthlySpreads =
        --            List.map
        --                (\monthlySpread_ ->
        --                    let
        --                        monthlySpread =
        --                            MonthlySpread.fromParseObject monthlySpread_
        --                    in
        --                    ( MonthlySpread.canonicalDate monthlySpread
        --                    , listItem
        --                        { listItemConfig
        --                            | additionalAttributes =
        --                                [ Html.Events.onClick
        --                                    (lift (MonthlySpreadClicked monthlySpread_))
        --                                ]
        --                        }
        --                        [ text (MonthlySpread.title monthlySpread) ]
        --                    )
        --                )
        --                model.monthlySpreads
        --
        --        dailySpreads =
        --            List.map
        --                (\dailySpread_ ->
        --                    let
        --                        dailySpread =
        --                            DailySpread.fromParseObject dailySpread_
        --                    in
        --                    ( DailySpread.canonicalDate dailySpread
        --                    , listItem
        --                        { listItemConfig
        --                            | additionalAttributes =
        --                                [ Html.Events.onClick
        --                                    (lift (DailySpreadClicked dailySpread_))
        --                                ]
        --                        }
        --                        [ text (DailySpread.title dailySpread) ]
        --                    )
        --                )
        --                model.dailySpreads
        collectionSpreads =
            List.map
                (\collectionSpread_ ->
                    let
                        collectionSpread =
                            CollectionSpread.fromParseObject collectionSpread_
                    in
                    ( CollectionSpread.canonicalDate config.timeZone collectionSpread
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
    [ config.topAppBar
        { title = "Table of Content"
        , menuIcon = Nothing
        , additionalSections = []
        }
    , Html.div
        [ class "screen screen--seamless table-of-content"
        , config.fixedAdjust
        ]
        [ Html.div [ class "screen__wrapper" ]
            [ Html.div
                [ class "table-of-content__primary" ]
                [ Html.h3
                    [ class "table-of-content__subtitle" ]
                    [ text "Collections will show here, so you can quickly find them."
                    ]
                ]
            , if List.isEmpty collectionSpreads then
                text ""

              else
                list
                    { listConfig
                        | additionalAttributes =
                            [ class "table-of-content__items-wrapper" ]
                    }
                    (List.map Tuple.second <|
                        List.sortBy Tuple.first <|
                            List.concat
                                [ collectionSpreads

                                -- , monthlySpreads
                                -- , dailySpreads
                                ]
                    )
            , fab
                { fabConfig
                    | onClick = Just (lift NewSpreadClicked)
                    , additionalAttributes =
                        [ class "screen__fab" ]
                }
                "add"
            ]
        ]
    , newSpreadDialog lift config model
    ]


newSpreadDialog : (Msg msg -> msg) -> Config msg -> Model -> Html msg
newSpreadDialog lift config model =
    let
        ( year, month, dayOfMonth ) =
            Calendar.toGregorian config.today

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
                            [ -- Html.Events.onClick (lift NewMonthlySpreadClicked)
                              Html.Attributes.tabindex 0
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
                            [ -- Html.Events.onClick (lift NewDailySpreadClicked)
                              Html.Attributes.tabindex 0
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
            [ textButton
                { buttonConfig
                    | onClick = Just (lift NewSpreadDialogClosed)
                }
                "Cancel"
            ]
        }
