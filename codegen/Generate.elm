module Generate exposing (main)

{-| -}

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Annotation exposing (Annotation)
import Elm.Case
import Gen.CodeGen.Generate as Generate exposing (Directory(..))
import Iso3166
import Json.Decode
import Json.Encode


main : Program Json.Encode.Value () ()
main =
    Generate.fromDirectory (\directory -> mainFile :: files directory)


mainFile : Elm.File
mainFile =
    let
        ann : Annotation
        ann =
            Annotation.named [] "CountryCode"
    in
    Elm.file [ "Cldr" ]
        [ allCountryCodes
            |> List.map Elm.variant
            |> Elm.customType "CountryCode"
            |> Elm.withDocumentation "All the supported country codes. `GT` and `LT` are defined in Basics so we define them as `GT_` and `LT_`."
            |> Elm.exposeWith { exposeConstructor = True, group = Nothing }
        , (\countryCodeExpr ->
            allCountryCodes
                |> List.map
                    (\countryCode ->
                        Elm.Case.branch0 countryCode
                            (countryCode
                                |> String.toLower
                                |> String.replace "_" ""
                                |> Elm.string
                            )
                    )
                |> Elm.Case.custom countryCodeExpr ann
          )
            |> Elm.fn ( "countryCodeExpr", Just ann )
            |> Elm.declaration "toAlpha2"
            |> Elm.withDocumentation "Two-letter `ISO 3166-1 alpha-2` code from `CountryCode`."
            |> Elm.expose
        , allCountryCodes
            |> List.map (\countryCode -> Elm.val countryCode)
            |> Elm.list
            |> Elm.withType (Annotation.list ann)
            |> Elm.declaration "all"
            |> Elm.withDocumentation "All `CountryCode`s sorted alphabetically."
            |> Elm.expose
        ]


files : Generate.Directory -> List Elm.File
files (Directory directory) =
    case Dict.get "en" directory.directories of
        Just (Directory en) ->
            case
                Dict.get "languages.json" en.files
            of
                Just languagesJson ->
                    case decodeLanguages languagesJson of
                        Ok languagesDict ->
                            directory.directories
                                |> Dict.toList
                                |> List.concatMap
                                    (\( key, Directory subdirectory ) ->
                                        if key == "und" then
                                            -- Unknown language
                                            []

                                        else
                                            case Dict.get key languagesDict of
                                                Just languageName ->
                                                    case splitLanguage languageName of
                                                        Just splatLanguageName ->
                                                            case Dict.get "territories.json" subdirectory.files of
                                                                Just territoriesJson ->
                                                                    case decodeTerritories key territoriesJson of
                                                                        Ok territories ->
                                                                            [ Elm.file ("Cldr" :: splatLanguageName)
                                                                                [ countryCodeToNameDeclaration languageName territories
                                                                                ]
                                                                            ]

                                                                        Err e ->
                                                                            error languageName <| "'territories.json' decoding failed: " ++ Json.Decode.errorToString e

                                                                Nothing ->
                                                                    error languageName <| "Couldn't split language name"

                                                        Nothing ->
                                                            error languageName <| "'territories.json' not found"

                                                Nothing ->
                                                    []
                                    )

                        Err e ->
                            error "Error" <| "'languages.json' decoding failed: " ++ Json.Decode.errorToString e

                Nothing ->
                    error "Error" "'languages.json' not found"

        Nothing ->
            error "Error" "'en' directory not found"


decodeTerritories : String -> String -> Result Json.Decode.Error (Dict String String)
decodeTerritories key input =
    let
        decoder =
            Json.Decode.at
                [ "main"
                , key
                , "localeDisplayNames"
                , "territories"
                ]
                (Json.Decode.dict Json.Decode.string)
    in
    Json.Decode.decodeString decoder input


allCountryCodes : List String
allCountryCodes =
    Iso3166.all
        |> List.map (Iso3166.toAlpha2 >> toUpper)
        |> (::) "XK"
        -- Kosovo
        |> List.sort


toUpper : String -> String
toUpper input =
    input
        |> String.toUpper
        |> String.replace "GT" "GT_"
        |> String.replace "LT" "LT_"


countryCodeToNameDeclaration : String -> Dict String String -> Elm.Declaration
countryCodeToNameDeclaration languageName territories =
    Elm.fn ( "countryCode", Just countryCodeAnnotation )
        (\countryCodeExpr ->
            allCountryCodes
                |> List.filterMap
                    (\countryCode ->
                        Dict.get (String.replace "_" "" countryCode) territories
                            |> Maybe.map
                                (\name ->
                                    Elm.Case.branch0 countryCode (Elm.string name)
                                )
                    )
                |> Elm.Case.custom countryCodeExpr countryCodeAnnotation
        )
        |> Elm.declaration "countryCodeToName"
        |> Elm.withDocumentation ("Name for `CountryCode` in " ++ languageName ++ ".")
        |> Elm.expose


countryCodeAnnotation : Annotation
countryCodeAnnotation =
    Annotation.namedWith [ "Cldr" ] "CountryCode" []


splitLanguage : String -> Maybe (List String)
splitLanguage lang =
    let
        suffixes =
            [ "Chinese"
            , "English"
            , "French"
            , "Gaelic"
            , "German"
            , "Pidgin"
            , "Portuguese"
            , "Spanish"
            , "Swahili"
            ]
    in
    case String.split " " lang of
        [ atom ] ->
            Just [ atom ]

        [ "Hindi", "(Latin)" ] ->
            Just [ "Hindi", "Latin" ]

        [ "Latin", "American", "Spanish" ] ->
            Just [ "Spanish", "LatinAmerican" ]

        [ "Swiss", "High", "German" ] ->
            Just [ "German", "Swiss" ]

        [ "Norwegian", suffix ] ->
            Just [ "Norwegian", suffix ]

        [ prefix, suffix ] ->
            if List.member suffix suffixes then
                Just [ suffix, prefix ]

            else
                Nothing

        _ ->
            Nothing


error : String -> String -> List Elm.File
error file msg =
    [ Elm.file [ file ]
        [ Elm.declaration "error" <|
            Elm.string msg
        ]
    ]


decodeLanguages : String -> Result Json.Decode.Error (Dict String String)
decodeLanguages input =
    let
        decoder =
            Json.Decode.at
                [ "main"
                , "en"
                , "localeDisplayNames"
                , "languages"
                ]
                (Json.Decode.dict Json.Decode.string)
    in
    input
        |> Json.Decode.decodeString decoder
