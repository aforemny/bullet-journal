module Type.Bullet.Parser exposing (parse)

import Parser exposing (..)
import String
import Type.Bullet as Bullet exposing (Bullet, emptyEvent, emptyNote, emptyTask)


parse : String -> Bullet
parse input =
    run parser input
        |> Result.toMaybe
        |> Maybe.withDefault
            { emptyTask
                | text = input
            }


parser =
    oneOf
        [ eventParser
        , taskParser
        , noteParser
        ]


bulletParser symbolParser ctor =
    oneOf
        [ succeed ctor
            |. symbolParser
            |= textParser
        ]


eventParser =
    bulletParser (symbol "O") (\text -> { emptyEvent | text = text })


uncheckedTaskParser =
    bulletParser
        (oneOf
            [ symbol "."
            , symbol "[]"
            , symbol "[ ]"
            ]
        )
        (\text -> { emptyTask | text = text })


checkedTaskParser =
    bulletParser
        (symbol "[x]")
        (\text ->
            { emptyTask | text = text, state = Bullet.Task Bullet.Checked }
        )


noteParser =
    bulletParser (symbol "-")
        (\text -> { emptyNote | text = text })


taskParser =
    oneOf
        [ checkedTaskParser
        , uncheckedTaskParser
        ]


spaces =
    chompWhile (\char -> List.member char [ ' ', '\t', '\u{000D}', '\n' ])


textParser =
    map String.trim (getChompedString (chompWhile (always True)))
