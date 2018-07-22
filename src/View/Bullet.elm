module View.Bullet exposing (..)

import Html.Attributes as Html
import Html exposing (Html, text)
import Material
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


init lift =
    Material.init (lift << Mdc)


subscriptions lift model =
    Material.subscriptions (lift << Mdc) model


update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model


view lift viewConfig model =
    Html.div
        [ Html.class "bullet"
        ]
        [ viewConfig.toolbar
            { additionalSections = []
            }
        ]
