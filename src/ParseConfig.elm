module ParseConfig exposing (parseConfig)

import Parse


parseConfig : Parse.Config
parseConfig =
    Parse.simpleConfig "bujo" "bujo"
        |> (\config ->
                { config | serverUrl = "/parse" }
           )
