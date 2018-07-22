module View.DailySpread exposing (..)

import Dict exposing (Dict)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Material
import Time.Calendar.Gregorian as Calendar
import Time.Calendar.MonthDay as Calendar
import Time.Calendar.OrdinalDate as Calendar
import Time.Calendar.Week as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.DailySpread as DailySpread
import View


type alias Model msg =
    { mdc : Material.Model msg
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    }


type Msg msg
    = Mdc (Material.Msg msg)



-- | BulletChanged Index String


type alias Index =
    Int


init lift =
  Cmd.none


subscriptions lift model =
  Material.subscriptions (lift << Mdc) model


update : (Msg msg -> msg) -> Msg msg -> Model msg -> ( Model msg, Cmd msg )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model



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
        dailySpread =
            DailySpread.empty 2018 7 22

        bullets =
            []
    in
        Html.div
            [ Html.class "daily-spread" ]
            [ viewConfig.toolbar
                { additionalSections = []
                }
            , Html.h1
                [ Html.class "daily-spread__title"
                ]
                [ text (DailySpread.title dailySpread)
                ]
            , Html.ol
                [ Html.class "daily-spread__bullet-wrapper"
                ]
                (List.indexedMap
                    (\index bullet ->
                        let
                            value =
                                case bullet of
                                    Bullet.Task { text } ->
                                        text

                                    Bullet.Event { text } ->
                                        text

                                    Bullet.Note { text } ->
                                        text
                        in
                            Bullet.view
                                { node = Html.li
                                , additionalAttributes =
                                    [ Html.class "daily-spread__bullet"
                                    ]
                                }
                                bullet
                    )
                    (bullets ++ [ Bullet.Note { text = "" } ])
                )
            ]
