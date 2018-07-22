module View exposing (..)

import Date exposing (Date)
import Html exposing (Html, text)
import Parse
import Time.Calendar.Days as Calendar


type alias Config msg =
    { toolbar : ToolbarConfig msg -> Html msg
    , today : Calendar.Day
    , now : Date
    , parse : Parse.Config
    }


type alias ToolbarConfig msg =
    { additionalSections : List (Html msg)
    }
