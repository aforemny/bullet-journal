module View.Index exposing (Model, Msg(..), defaultModel, init, newSpreadDialog, subscriptions, update, view)

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes as Html
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Dialog as Dialog
import Material.Fab as Fab
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Toolbar as Toolbar
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


type alias Model msg =
    { mdc : Material.Model msg
    , monthlySpreads : List (Parse.Object MonthlySpread)
    , dailySpreads : List (Parse.Object DailySpread)
    , collectionSpreads : List (Parse.Object CollectionSpread)
    , error : Maybe Parse.Error
    , showNewSpreadDialog : Bool
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    , monthlySpreads = []
    , dailySpreads = []
    , collectionSpreads = []
    , error = Nothing
    , showNewSpreadDialog = False
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | MonthlySpreadsResult (Result Parse.Error (List (Parse.Object MonthlySpread)))
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


init : (Msg msg -> msg) -> View.Config msg -> Model msg -> ( Model msg, Cmd msg )
init lift viewConfig model =
    ( { defaultModel
        | monthlySpreads = model.monthlySpreads
        , dailySpreads = model.dailySpreads
        , collectionSpreads = model.collectionSpreads
      }
    , Cmd.batch
        [ Material.init (lift << Mdc)
        , Parse.send viewConfig.parse
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


subscriptions : (Msg msg -> msg) -> Model msg -> Sub msg
subscriptions lift model =
    Material.subscriptions (lift << Mdc) model


update :
    (Msg msg -> msg)
    -> View.Config msg
    -> Msg msg
    -> Model msg
    -> ( Model msg, Cmd msg )
update lift viewConfig msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

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


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
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
                    , Lists.li
                        [ Options.onClick
                            (lift (MonthlySpreadClicked monthlySpread_))
                        ]
                        [ text (MonthlySpread.title monthlySpread)
                        ]
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
                    , Lists.li
                        [ Options.onClick (lift (DailySpreadClicked dailySpread_))
                        ]
                        [ text (DailySpread.title dailySpread)
                        ]
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
                    , Lists.li
                        [ Options.onClick
                            (lift (CollectionSpreadClicked collectionSpread_))
                        ]
                        [ text (CollectionSpread.title collectionSpread)
                        ]
                    )
                )
                model.collectionSpreads
    in
    Html.div
        [ Html.class "index"
        ]
        [ viewConfig.toolbar
            { title = "Index"
            , menuIcon =
                Icon.view
                    [ Toolbar.menuIcon
                    ]
                    "menu"
            , additionalSections = []
            }
        , Card.view
            [ cs "index__wrapper"
            ]
            [ Html.div
                [ Html.class "index__primary" ]
                [ Html.h2
                    [ Html.class "index__title" ]
                    [ text "Index" ]
                , Html.h3
                    [ Html.class "index__subtitle" ]
                    [ text "Collections will show here, so you can quickly find them." ]
                ]
            , Lists.ol (lift << Mdc)
                -- TODO: id
                ""
                model.mdc
                [ cs "index__items-wrapper" ]
                (List.map Tuple.second <|
                    List.sortBy Tuple.first <|
                        List.concat
                            [ monthlySpreads
                            , dailySpreads
                            , collectionSpreads
                            ]
                )
            , Fab.view (lift << Mdc)
                "index__new-spread"
                model.mdc
                [ cs "index__new-spread"
                , Fab.ripple
                , Options.onClick (lift NewSpreadClicked)
                ]
                [ text "add" ]
            ]
        , newSpreadDialog lift viewConfig model
        ]


newSpreadDialog : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
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
    Dialog.view (lift << Mdc)
        "new-spread-dialog"
        model.mdc
        [ Dialog.onClose (lift NewSpreadDialogClosed)
        , when model.showNewSpreadDialog Dialog.open
        ]
        [ Dialog.content []
            [ Lists.ul (lift << Mdc)
                -- TODO: id
                ""
                model.mdc
                [ Lists.twoLine
                ]
                [ Lists.li
                    [ Options.onClick (lift NewMonthlySpreadClicked)
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
                    [ Options.onClick (lift NewDailySpreadClicked)
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
                    [ Options.onClick (lift NewCollectionSpreadClicked)
                    , Options.attribute (Html.tabindex 0)
                    ]
                    [ Lists.text []
                        [ text "New collection"
                        ]
                    ]
                ]
            ]
        , Dialog.actions []
            [ Button.view (lift << Mdc)
                "new-spread-dialog__cancel"
                model.mdc
                [ Button.ripple
                , Button.onClick (lift NewSpreadDialogClosed)
                , Dialog.cancel
                ]
                [ text "Cancel"
                ]
            ]
        ]
