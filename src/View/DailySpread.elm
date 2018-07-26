module View.DailySpread exposing (..)

import Dict exposing (Dict)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Material
import Material.Button as Button
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



-- | BulletChanged Index String


type alias Index =
    Int


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
            (Bullet.get viewConfig.parse "DailySpread" objectId)
        ]
    )


subscriptions : (Msg msg -> msg) -> Model msg -> Sub msg
subscriptions lift model =
    Material.subscriptions (lift << Mdc) model


update : (Msg msg -> msg) -> Msg msg -> Model msg -> ( Model msg, Cmd msg )
update lift msg model =
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
            ( model
            , model.dailySpread
                |> Maybe.map (.objectId >> Bullet.anyObjectId)
                |> Maybe.map (Url.NewBullet "daily-spread" "DailySpread")
                |> Maybe.map (Navigation.newUrl << Url.toString)
                |> Maybe.withDefault Cmd.none
            )



--        BulletChanged index input ->
--            let
--                numBullets =
--                    List.length model.bullets
--            in
--                (model.bullets
--                    |> \bullets ->
--                        if numBullets < index + 1 then
--                            bullets ++ List.repeat (index - numBullets + 1) (Bullet.Note { text = "" })
--                        else
--                            bullets
--                )
--                    |> List.indexedMap
--                        (\otherIndex bullet ->
--                            if otherIndex == index then
--                                Bullet.Note { text = input }
--                            else
--                                bullet
--                        )
--                    |> \bullets ->
--                        ( { model | bullets = bullets }, Cmd.none )


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    let
        dailySpread_ =
            model.dailySpread

        dailySpread =
            dailySpread_
                |> Maybe.map DailySpread.fromParseObject

        bullets =
            model.bullets
    in
        Html.div
            [ Html.class "daily-spread" ]
            [ viewConfig.toolbar
                { additionalSections =
                    [ Toolbar.section
                        [ Toolbar.alignEnd
                        ]
                        [ Button.view (lift << Mdc)
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
            , Html.h1
                [ Html.class "daily-spread__title"
                ]
                [ (dailySpread
                    |> Maybe.map DailySpread.title
                    |> Maybe.withDefault ""
                    |> text
                  )
                ]
            , Html.ol
                [ Html.class "daily-spread__bullet-wrapper"
                ]
                (List.indexedMap
                    (\index bullet ->
                        Bullet.view
                            { node = Html.li
                            , additionalAttributes =
                                [ Html.class "daily-spread__bullet"
                                ]
                            }
                            (Bullet.fromParseObject bullet)
                    )
                    bullets
                )
            ]
