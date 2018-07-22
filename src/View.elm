module View exposing (..)

import Html exposing (Html, text)


type alias Config msg =
    { toolbar : ToolbarConfig msg -> Html msg
    }


type alias ToolbarConfig msg =
    { additionalSections : List (Html msg)
    }
