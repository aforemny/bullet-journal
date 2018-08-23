module View.Start exposing (..)

import Date exposing (Date)
import Html exposing (Html, text)
import Html.Attributes as Html
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Material
import Material.Card as Card
import Material.Chip as Chip
import Material.List as Lists
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Textfield as TextField
import Navigation
import Parse
import Private.ObjectId as ObjectId
import Task exposing (Task)
import Time.Calendar.Gregorian as Calendar
import Time.Format.Locale as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.Bullet.Parser as Bullet
import Type.DailySpread as DailySpread exposing (DailySpread)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import Url exposing (Url)
import View


type alias Model msg =
    { mdc : Material.Model msg
    , bullets : List (Parse.Object Bullet)
    , input : String
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    , bullets = []
    , input = ""
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | NoOp
    | BulletsChanged (Result Parse.Error (List (Parse.Object Bullet)))
    | BulletClicked (Parse.Object Bullet)
    | InputChanged String
    | InputSubmitted String
    | BulletCreated (Result Parse.Error (Parse.Object Bullet))
    | BulletMarkedDone (Result Parse.Error (Parse.ObjectId Bullet))


init : (Msg msg -> msg) -> View.Config msg -> Model msg -> ( Model msg, Cmd msg )
init lift { today, parse } model =
    let
        ( year, month, dayOfMonth ) =
            Calendar.toGregorian today

        getBullets =
            Parse.toTask parse
                (Parse.query Bullet.decode
                    (Parse.emptyQuery "Bullet"
                     --                        |> (\query ->
                     --                                { query
                     --                                    | whereClause =
                     --                                        Parse.and
                     --                                            [ Parse.equalTo "year" (Encode.int year)
                     --                                            , Parse.equalTo "month" (Encode.int month)
                     --                                            ]
                     --                                }
                     --                           )
                    )
                )
    in
    ( defaultModel
    , Cmd.batch
        [ Task.attempt (lift << BulletsChanged) getBullets
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
update lift { today, parse } msg model =
    case Debug.log "Msg" msg of
        BulletCreated (Err err) ->
            -- TODO:
            let
                _ =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        BulletCreated (Ok bullet) ->
            ( { model | bullets = bullet :: model.bullets }, Cmd.none )

        InputSubmitted input ->
            let
                ( year, month, dayOfMonth ) =
                    Calendar.toGregorian today

                bullet =
                    Bullet.parse input
                        |> (\bullet ->
                                { bullet
                                    | year = Just year
                                    , month = Just month
                                    , dayOfMonth = Just dayOfMonth
                                }
                           )
            in
            ( { model | input = "" }
            , Task.attempt (lift << BulletCreated)
                (Bullet.create parse bullet
                    |> Task.andThen (Bullet.get parse << .objectId)
                )
            )

        InputChanged input ->
            ( { model | input = input }, Cmd.none )

        BulletMarkedDone (Err err) ->
            -- TODO:
            let
                _ =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        BulletMarkedDone (Ok objectId) ->
            let
                bullets =
                    model.bullets
                        |> List.filter ((/=) objectId << .objectId)
            in
            ( { model | bullets = bullets }, Cmd.none )

        BulletClicked bullet ->
            case bullet.state of
                Bullet.Task Bullet.Unchecked ->
                    ( model
                    , Task.attempt (lift << BulletMarkedDone)
                        (Bullet.update parse
                            bullet.objectId
                            (Bullet.fromParseObject bullet
                                |> (\bullet ->
                                        { bullet | state = Bullet.Task Bullet.Checked }
                                   )
                            )
                            |> Task.map (always bullet.objectId)
                        )
                    )

                _ ->
                    ( model, Cmd.none )

        BulletsChanged (Err err) ->
            -- TODO:
            let
                _ =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        BulletsChanged (Ok bullets) ->
            ( { model | bullets = bullets }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift ({ today } as viewConfig) model =
    let
        sortedBullets =
            model.bullets
                |> List.filter
                    (\bullet ->
                        List.all identity
                            [ bullet.state /= Bullet.Note
                            , bullet.state /= Bullet.Task Bullet.Checked
                            ]
                    )
                |> List.sortBy
                    (\bullet ->
                        let
                            stateSort =
                                case bullet.state of
                                    Bullet.Task _ ->
                                        0

                                    Bullet.Note ->
                                        1

                                    Bullet.Event ->
                                        2

                            createdAtSort =
                                Date.toTime bullet.createdAt
                        in
                        ( stateSort, createdAtSort )
                    )
    in
    Html.div
        [ Html.class "start"
        ]
        [ inputCard lift viewConfig model
        , dailyBulletsCard lift viewConfig model sortedBullets
        , monthlyBulletsCard lift viewConfig model sortedBullets
        , upcomingEventsCard lift viewConfig model sortedBullets
        , backlogCard lift viewConfig model sortedBullets
        ]


inputCard lift viewConfig model =
    Html.div []
        [ Card.view
            [ cs "start__input-card" ]
            [ TextField.view (lift << Mdc)
                "start__input"
                model.mdc
                [ TextField.placeholder "Enter bullet"
                , TextField.fullwidth
                , TextField.value model.input
                , TextField.nativeControl
                    [ Options.on "keypress"
                        (Decode.map (lift << InputSubmitted)
                            (Decode.at [ "which" ] Decode.int
                                |> Decode.andThen
                                    (\which ->
                                        if which == 13 then
                                            Html.Events.targetValue
                                        else
                                            Decode.fail ""
                                    )
                            )
                        )
                    ]
                , Options.onInput (lift << InputChanged)
                ]
                []
            ]
        , let
            bullet =
                Bullet.parse model.input
          in
          Chip.chipset []
            (List.filterMap identity
                [ Just <|
                    Chip.view (lift << Mdc)
                        "my-chip"
                        model.mdc
                        []
                        [ text
                            (case bullet.state of
                                Bullet.Task _ ->
                                    "Task"

                                Bullet.Event ->
                                    "Event"

                                Bullet.Note ->
                                    "Note"
                            )
                        ]
                , Just <|
                    Chip.view (lift << Mdc) "my-chip" model.mdc [] [ text bullet.text ]
                ]
            )
        ]


dailyBulletsCard lift ({ today } as viewConfig) model sortedBullets =
    let
        bullets =
            sortedBullets
                |> List.filter
                    (\bullet ->
                        (bullet.year == Just year)
                            && (bullet.month == Just month)
                            && (bullet.dayOfMonth == Just dayOfMonth)
                    )

        title =
            String.join " "
                [ case Calendar.defaultTimeLocale of
                    Calendar.TimeLocale { months } ->
                        List.drop (month - 1) months
                            |> List.head
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault ""
                , toString dayOfMonth
                ]

        ( year, month, dayOfMonth ) =
            Calendar.toGregorian today
    in
    Card.view
        [ cs "start__daily-bullets"
        ]
        [ Html.h3
            [ Html.class "start__daily-bullets__title" ]
            [ text title ]
        , Lists.ul []
            (List.map (viewBullet lift viewConfig model) bullets)
        ]


monthlyBulletsCard lift ({ today } as viewConfig) model sortedBullets =
    let
        bullets =
            sortedBullets
                |> List.filter
                    (\bullet ->
                        (bullet.year == Just year)
                            && (bullet.month == Just month)
                            && (Maybe.withDefault 0 bullet.dayOfMonth > dayOfMonth)
                    )

        title =
            String.join " "
                [ case Calendar.defaultTimeLocale of
                    Calendar.TimeLocale { months } ->
                        List.drop (month - 1) months
                            |> List.head
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault ""
                ]

        ( year, month, dayOfMonth ) =
            Calendar.toGregorian today
    in
    Card.view
        [ cs "start__monthly-bullets"
        ]
        [ Html.h3
            [ Html.class "start__daily-bullets__title" ]
            [ text title ]
        , Lists.ul []
            (List.map (viewBullet lift viewConfig model) bullets)
        ]


upcomingEventsCard lift ({ today } as viewConfig) model sortedBullets =
    let
        bullets =
            sortedBullets
                |> List.filter
                    (\bullet ->
                        List.all identity
                            [ bullet.state == Bullet.Event
                            , Maybe.withDefault 0 bullet.year >= year
                            , Maybe.withDefault 0 bullet.month >= month
                            , Maybe.withDefault 0 bullet.dayOfMonth > dayOfMonth
                            ]
                    )

        title =
            "Upcoming events"

        ( year, month, dayOfMonth ) =
            Calendar.toGregorian today
    in
    Card.view
        [ cs "start__monthly-bullets"
        ]
        [ Html.h3
            [ Html.class "start__daily-bullets__title" ]
            [ text title ]
        , Lists.ul []
            (List.map (viewBullet lift viewConfig model) bullets)
        ]


backlogCard lift ({ today } as viewConfig) model sortedBullets =
    let
        bullets =
            sortedBullets
                |> List.filter
                    (\bullet ->
                        List.all identity
                            [ bullet.state == Bullet.Task Bullet.Unchecked
                            , ( Maybe.withDefault 0 bullet.year
                              , Maybe.withDefault 0 bullet.month
                              , Maybe.withDefault 0 bullet.dayOfMonth
                              )
                                < ( year, month, dayOfMonth )
                            ]
                    )

        title =
            "Backlog"

        ( year, month, dayOfMonth ) =
            Calendar.toGregorian today
    in
    Card.view
        [ cs "start__monthly-bullets"
        ]
        [ Html.h3
            [ Html.class "start__daily-bullets__title" ]
            [ text title ]
        , Lists.ul []
            (List.map (viewBullet lift viewConfig model) bullets)
        ]


bulletClass : Parse.Object Bullet -> String
bulletClass bullet =
    String.join " "
        [ "bullet"
        , case bullet.state of
            Bullet.Event ->
                "bullet--event"

            Bullet.Note ->
                "bullet--note"

            Bullet.Task Bullet.Unchecked ->
                "bullet--task bullet--task--unchecked"

            Bullet.Task Bullet.Checked ->
                "bullet--task bullet--task--checked"

            Bullet.Task Bullet.Migrated ->
                "bullet--task bullet--task--migrated"
        ]


viewBullet :
    (Msg msg -> msg)
    -> View.Config msg
    -> Model msg
    -> Parse.Object Bullet
    -> Html msg
viewBullet lift viewConfig model bullet =
    let
        date =
            case ( bullet.year, bullet.month, bullet.dayOfMonth ) of
                ( Just year, Just month, Just day ) ->
                    Just (toString day ++ ". " ++ toString month ++ " " ++ toString year)

                _ ->
                    Nothing
    in
    Lists.li
        [ cs (bulletClass bullet)
        , Options.onClick (lift (BulletClicked bullet))
        ]
        [ Lists.graphicIcon [] ""
        , Lists.text []
            [ text bullet.text
            , Lists.secondaryText []
                [ text (Maybe.withDefault "–" date) ]
            ]
        ]
