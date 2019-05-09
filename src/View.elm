module View exposing (Config, ToolbarConfig)

import Browser.Navigation
import Html exposing (Html, text)
import Parse
import Time
import Time.Calendar.Days as Calendar


type alias Config msg =
    { toolbar : ToolbarConfig msg -> Html msg
    , today : Calendar.Day
    , now : Time.Posix
    , parse : Parse.Config
    , key : Browser.Navigation.Key
    , timeZone : Time.Zone
    }


type alias ToolbarConfig msg =
    { title : String
    , menuIcon : Html msg
    , additionalSections : List (Html msg)
    }
