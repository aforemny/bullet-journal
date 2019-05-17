module Screen.DailySpread exposing (Model, Msg(..), defaultModel, init, subscriptions, update, view)

import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Material.Button exposing (buttonConfig, raisedButton, textButton)
import Material.Card exposing (card, cardBlock, cardConfig)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.List exposing (list, listConfig)
import Material.TopAppBar as TopAppBar
import Parse
import Route exposing (Route)
import Screen
import Task
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar
import Type.Bullet as Bullet exposing (Bullet)


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
    | BackClicked
    | BulletClicked (Parse.ObjectId Bullet)


init :
    (Msg msg -> msg)
    -> Screen.Config msg
    ->
        { year : Int
        , month : Int
        , dayOfMonth : Int
        }
    -> Model
    -> ( Model, Cmd msg )
init lift viewConfig ({ year, month, dayOfMonth } as date) model =
    ( defaultModel date
    , Cmd.batch
        [ Parse.send viewConfig.parse
            (lift << BulletsResult)
            (Parse.query Bullet.decode (Parse.emptyQuery "Bullet"))
        ]
    )


subscriptions : (Msg msg -> msg) -> Model -> Sub msg
subscriptions lift model =
    Sub.none


update :
    (Msg msg -> msg)
    -> Screen.Config msg
    -> Msg msg
    -> Model
    -> ( Model, Cmd msg )
update lift viewConfig msg model =
    case msg of
        BulletsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        BulletsResult (Ok bullets) ->
            ( { model | bullets = bullets }, Cmd.none )

        NewBulletClicked ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.EditBullet Nothing))
            )

        BulletClicked bulletId ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.EditBullet (Just bulletId)))
            )

        BackClicked ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString Route.Start)
            )


view : (Msg msg -> msg) -> Screen.Config msg -> Model -> List (Html msg)
view lift viewConfig model =
    let
        title =
            String.fromInt model.date.year
                ++ "-"
                ++ String.fromInt
                    model.date.month
                ++ "-"
                ++ String.fromInt
                    model.date.dayOfMonth
    in
    [ viewConfig.topAppBar
        { title = title
        , menuIcon =
            Just <|
                iconButton
                    { iconButtonConfig
                        | onClick = Just (lift BackClicked)
                        , additionalAttributes = [ TopAppBar.navigationIcon ]
                    }
                    "arrow_back"
        , additionalSections =
            [ TopAppBar.section [ TopAppBar.alignEnd ]
                [ textButton
                    { buttonConfig
                        | onClick = Just (lift NewBulletClicked)
                    }
                    "New bullet"
                ]
            ]
        }
    , Html.div
        [ class "daily-spread", viewConfig.fixedAdjust ]
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
                                model.bullets
                            )
                        ]
                ]
            , actions = Nothing
            }
        ]
    ]
