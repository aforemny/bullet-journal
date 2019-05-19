module Screen.DailySpread exposing
    ( Model
    , Msg(..)
    , defaultModel
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Material.Button exposing (buttonConfig, raisedButton, textButton)
import Material.Card exposing (card, cardBlock, cardConfig)
import Material.Fab exposing (fab, fabConfig)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.List exposing (list, listConfig)
import Material.TopAppBar as TopAppBar
import Parse
import Route exposing (Route)
import Task
import Time
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar
import Type.Bullet as Bullet exposing (Bullet)


type alias Config msg =
    { key : Browser.Navigation.Key
    , parse : Parse.Config
    , topAppBar :
        { title : String
        , menuIcon : Maybe (Html msg)
        , additionalSections : List (Html msg)
        }
        -> Html msg
    , fixedAdjust : Html.Attribute msg
    }


type alias Model =
    { date : { year : Int, month : Int, dayOfMonth : Int }
    , bullets : List (Parse.Object Bullet)
    , error : Maybe Parse.Error
    }


defaultModel : { year : Int, month : Int, dayOfMonth : Int } -> Model
defaultModel date =
    { date = date
    , bullets = []
    , error = Nothing
    }


type Msg msg
    = BulletsResult (Result Parse.Error (List (Parse.Object Bullet)))
    | NewBulletClicked
    | BulletClicked (Parse.ObjectId Bullet)


init :
    (Msg msg -> msg)
    -> Config msg
    ->
        { year : Int
        , month : Int
        , dayOfMonth : Int
        }
    -> ( Model, Cmd msg )
init lift config ({ year, month, dayOfMonth } as date) =
    ( defaultModel date
    , Cmd.batch
        [ Parse.send config.parse
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
        BulletsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        BulletsResult (Ok bullets) ->
            ( { model | bullets = bullets }, Cmd.none )

        NewBulletClicked ->
            ( model
            , Browser.Navigation.pushUrl config.key
                (Route.toString (Route.EditBullet Nothing))
            )

        BulletClicked bulletId ->
            ( model
            , Browser.Navigation.pushUrl config.key
                (Route.toString (Route.EditBullet (Just bulletId)))
            )


view : (Msg msg -> msg) -> Config msg -> Model -> List (Html msg)
view lift config model =
    let
        title =
            String.fromInt model.date.year
                ++ "-"
                ++ String.fromInt
                    model.date.month
                ++ "-"
                ++ String.fromInt
                    model.date.dayOfMonth

        sortedBullets =
            model.bullets
                |> List.filter
                    (\bullet ->
                        case bullet.date of
                            Bullet.MonthDate { year, month } ->
                                year == model.date.year && month == model.date.month

                            Bullet.DayDate { year, month, dayOfMonth } ->
                                (year == model.date.year)
                                    && (month == model.date.month)
                                    && (dayOfMonth == model.date.dayOfMonth)
                    )
                |> List.sortBy
                    (\bullet ->
                        let
                            stateSort =
                                case bullet.ctor of
                                    Bullet.Task ->
                                        0

                                    Bullet.Note ->
                                        1

                                    Bullet.Event ->
                                        2

                            createdAtSort =
                                Time.posixToMillis bullet.createdAt
                        in
                        ( stateSort, createdAtSort )
                    )
    in
    [ config.topAppBar
        { title = title
        , menuIcon = Nothing
        , additionalSections = []
        }
    , Html.div
        [ class "daily-spread"
        , class "screen screen--scrollable"
        , config.fixedAdjust
        ]
        [ Html.div [ class "screen__wrapper" ]
            [ card
                { cardConfig
                    | additionalAttributes = [ class "daily-spread__wrapper" ]
                }
                { blocks =
                    [ cardBlock <|
                        Html.div []
                            [ Html.div [ class "daily-spread__primary" ]
                                [ Html.div [ class "daily-spread__title" ] [ text title ]
                                , Html.div [ class "daily-spread__subtitle" ]
                                    [ text "The Daily Log is designed for day-to-day use." ]
                                ]
                            , list
                                { listConfig
                                    | additionalAttributes =
                                        [ class "daily-spread__bullet-wrapper" ]
                                }
                                (List.map
                                    (\bullet ->
                                        Bullet.view
                                            { additionalOptions =
                                                [ class "daily-spread__bullet"
                                                , Html.Events.onClick (lift (BulletClicked bullet.objectId))
                                                ]
                                            }
                                            (Bullet.fromParseObject bullet)
                                    )
                                    sortedBullets
                                )
                            , fab
                                { fabConfig
                                    | onClick = Just (lift NewBulletClicked)
                                    , additionalAttributes =
                                        [ class "screen__fab" ]
                                }
                                "add"
                            ]
                    ]
                , actions = Nothing
                }
            ]
        ]
    ]
