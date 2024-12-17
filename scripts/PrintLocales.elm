module PrintLocales exposing (run)

import BackendTask
import Cldr
import Json.Encode
import Pages.Script as Script exposing (Script)


run : Script
run =
    [ Cldr.allNontrivialLocales
        -- |> List.filter
        --     (\locale ->
        --         List.member locale
        --             [ "ar-LY", "de-AT", "en-AU", "en", "fr-CA", "fr", "it", "ja" ]
        --     )
        |> List.map
            (\locale ->
                [ ( "id", Just locale )
                , ( "english", Cldr.localeToEnglishName locale )
                , ( "native", Cldr.localeToNativeName locale )
                ]
                    |> List.filterMap (\( k, v ) -> Maybe.map (Tuple.pair k << Json.Encode.string) v)
            )
        |> Json.Encode.list Json.Encode.object
        |> Json.Encode.encode 2
    , Cldr.allNontrivialLocales
        |> Json.Encode.list Json.Encode.string
        |> Json.Encode.encode 0
    ]
        |> List.map Script.log
        |> List.reverse
        |> BackendTask.combine
        |> BackendTask.map (\_ -> ())
        |> Script.withoutCliOptions
