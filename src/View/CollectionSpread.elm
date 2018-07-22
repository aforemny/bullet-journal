module View.CollectionSpread exposing (..)

import Date exposing (Date)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Material
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
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



--    | TitleChanged String
--    | BulletChanged Index String


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



--        TitleChanged title ->
--            ( { model | title = title }, Cmd.none )
--
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
      collectionSpread =
        CollectionSpread.empty "foobar" (Date.fromTime 0)
      bullets =
        []
  in
    Html.div
        [ Html.class "collection-spread"
        ]
        [ viewConfig.toolbar
            { additionalSections = []
            }
        , Html.div
            [ Html.class "collection-spread__title"
            ]
            [ Html.input
                [ Html.value (CollectionSpread.title collectionSpread)
                -- , Html.onInput (lift << TitleChanged)
                ]
                []
            ]
        , Html.ol
            []
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
                                [ Html.class "collection-spread__bullet"
                                ]
                            }
                            bullet
                )
                (bullets ++ [ Bullet.Note { text = "" } ])
            )
        ]
