module Screen.DailySpread exposing (Model, Msg(..), defaultModel, init, subscriptions, update, view)

import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Material.Button exposing (buttonConfig, raisedButton, textButton)
import Material.Card exposing (card, cardBlock, cardConfig)
import Material.Icon exposing (icon, iconConfig)
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
import Type.DailySpread as DailySpread exposing (DailySpread)


type alias Model =
    { dailySpread : Maybe (Parse.Object DailySpread)
    , bullets : List (Parse.Object Bullet)
    , error : Maybe Parse.Error
    }


defaultModel : Model
defaultModel =
    { dailySpread = Nothing
    , bullets = []
    , error = Nothing
    }


type Msg msg
    = DailySpreadResult (Result Parse.Error (Parse.Object DailySpread))
    | BulletsResult (Result Parse.Error (List (Parse.Object Bullet)))
    | NewBulletClicked
    | EditClicked
    | BackClicked
    | BulletClicked (Parse.ObjectId Bullet)


init :
    (Msg msg -> msg)
    -> Screen.Config msg
    -> Parse.ObjectId DailySpread
    -> Model
    -> ( Model, Cmd msg )
init lift viewConfig objectId model =
    ( defaultModel
    , Cmd.batch
        [ Task.attempt (lift << DailySpreadResult)
            (DailySpread.get viewConfig.parse objectId)
        , Task.attempt (lift << BulletsResult)
            (Bullet.getOf viewConfig.parse "DailySpread" objectId)
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
        DailySpreadResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DailySpreadResult (Ok dailySpread) ->
            ( { model | dailySpread = Just dailySpread }, Cmd.none )

        BulletsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        BulletsResult (Ok bullets) ->
            ( { model | bullets = bullets }, Cmd.none )

        NewBulletClicked ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.EditBullet Nothing))
            )

        EditClicked ->
            ( model
            , model.dailySpread
                |> Maybe.map (Route.EditDailySpread << .objectId)
                |> Maybe.map (Browser.Navigation.pushUrl viewConfig.key << Route.toString)
                |> Maybe.withDefault Cmd.none
            )

        BulletClicked bulletId ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.EditBullet (Just bulletId)))
            )

        BackClicked ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString Route.TableOfContent)
            )


view : (Msg msg -> msg) -> Screen.Config msg -> Model -> List (Html msg)
view lift viewConfig model =
    let
        dailySpread_ =
            model.dailySpread

        dailySpread =
            dailySpread_
                |> Maybe.map DailySpread.fromParseObject

        title =
            dailySpread
                |> Maybe.map DailySpread.title
                |> Maybe.withDefault ""

        bullets =
            model.bullets
    in
    [ viewConfig.topAppBar
        { title = title
        , menuIcon =
            icon
                { iconConfig
                    | additionalAttributes =
                        [ TopAppBar.navigationIcon
                        , Html.Events.onClick (lift BackClicked)
                        ]
                }
                "arrow_back"
        , additionalSections =
            [ TopAppBar.section [ TopAppBar.alignEnd ]
                [ textButton
                    { buttonConfig
                        | onClick = Just (lift EditClicked)
                    }
                    "Edit"
                , textButton
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
                                bullets
                            )
                        ]
                ]
            , actions = Nothing
            }
        ]
    ]
