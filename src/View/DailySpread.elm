module View.DailySpread exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes as Html
import Html.Events as Html
import Json.Decode as Decode exposing (Decoder)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Toolbar as Toolbar
import Navigation
import Parse
import Task
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.DailySpread as DailySpread exposing (DailySpread)
import Url exposing (Url)
import View


type alias Model msg =
    { mdc : Material.Model msg
    , dailySpread : Maybe (Parse.Object DailySpread)
    , bullets : List (Parse.Object Bullet)
    , error : Maybe Parse.Error
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    , dailySpread = Nothing
    , bullets = []
    , error = Nothing
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | DailySpreadResult (Result Parse.Error (Parse.Object DailySpread))
    | BulletsResult (Result Parse.Error (List (Parse.Object Bullet)))
    | NewBulletClicked
    | EditClicked
    | BackClicked
    | BulletClicked (Parse.ObjectId Bullet)


init :
    (Msg msg -> msg)
    -> View.Config msg
    -> Parse.ObjectId DailySpread
    -> Model msg
    -> ( Model msg, Cmd msg )
init lift viewConfig objectId model =
    ( defaultModel
    , Cmd.batch
        [ Material.init (lift << Mdc)
        , Task.attempt (lift << DailySpreadResult)
            (DailySpread.get viewConfig.parse objectId)
        , Task.attempt (lift << BulletsResult)
            (Bullet.getOf viewConfig.parse "DailySpread" objectId)
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

        DailySpreadResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        DailySpreadResult (Ok dailySpread) ->
            ( { model | dailySpread = Just dailySpread }, Cmd.none )

        BulletsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        BulletsResult (Ok bullets) ->
            ( { model | bullets = bullets }, Cmd.none )

        NewBulletClicked ->
            ( model, Navigation.newUrl (Url.toString (Url.EditBullet Nothing)) )

        EditClicked ->
            ( model
            , model.dailySpread
                |> Maybe.map (Url.EditDailySpread << .objectId)
                |> Maybe.map (Navigation.newUrl << Url.toString)
                |> Maybe.withDefault Cmd.none
            )

        BulletClicked bulletId ->
            ( model
            , Navigation.newUrl (Url.toString (Url.EditBullet (Just bulletId)))
            )

        BackClicked ->
            ( model
            , Navigation.newUrl (Url.toString Url.Index)
            )


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
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
    Html.div
        [ Html.class "daily-spread" ]
        [ viewConfig.toolbar
            { title =
                title
            , menuIcon =
                Icon.view
                    [ Toolbar.menuIcon
                    , Options.onClick (lift BackClicked)
                    ]
                    "arrow_back"
            , additionalSections =
                [ Toolbar.section
                    [ Toolbar.alignEnd
                    ]
                    [ Button.view (lift << Mdc)
                        "daily-spread__edit-daily-spread"
                        model.mdc
                        [ Button.ripple
                        , Button.onClick (lift EditClicked)
                        ]
                        [ text "Edit"
                        ]
                    , Button.view (lift << Mdc)
                        "daily-spread__new-bullet"
                        model.mdc
                        [ Button.ripple
                        , Button.onClick (lift NewBulletClicked)
                        ]
                        [ text "New bullet"
                        ]
                    ]
                ]
            }
        , Card.view
            [ cs "daily-spread__wrapper" ]
            [ Html.div
                [ Html.class "daily-spread__primary" ]
                [ Html.div
                    [ Html.class "daily-spread__title" ]
                    [ text title ]
                , Html.div
                    [ Html.class "daily-spread__subtitle" ]
                    [ text "The Daily Log is designed for day-to-day use." ]
                ]
            , Lists.ol
                [ cs "daily-spread__bullet-wrapper"
                ]
                (List.map
                    (\bullet ->
                        Bullet.view
                            { additionalOptions =
                                [ cs "daily-spread__bullet"
                                , Options.onClick (lift (BulletClicked bullet.objectId))
                                ]
                            }
                            (Bullet.fromParseObject bullet)
                    )
                    bullets
                )
            ]
        ]
