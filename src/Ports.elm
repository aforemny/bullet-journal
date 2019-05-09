port module Ports exposing (now, nowUnsafe, readDateUnsafe, readDayUnsafe, today, todayUnsafe)

import Iso8601
import Json.Decode exposing (Value)
import Time
import Time.Calendar.Days as Calendar
import Time.Calendar.Gregorian as Calendar


port todayUnsafe : (String -> msg) -> Sub msg


today : (Maybe Calendar.Day -> msg) -> Sub msg
today lift =
    todayUnsafe (lift << readDayUnsafe)


readDayUnsafe : String -> Maybe Calendar.Day
readDayUnsafe dayString =
    case
        dayString
            |> String.split "-"
            |> List.map String.toInt
    of
        [ Just year, Just month, Just dayOfMonth ] ->
            Just (Calendar.fromGregorian year month dayOfMonth)

        _ ->
            Nothing


port nowUnsafe : (String -> msg) -> Sub msg


now : (Maybe Time.Posix -> msg) -> Sub msg
now lift =
    nowUnsafe (lift << readDateUnsafe)


readDateUnsafe : String -> Maybe Time.Posix
readDateUnsafe dateString =
    case Iso8601.toTime dateString of
        Ok date ->
            Just date

        Err _ ->
            Nothing
