module Generate exposing (main)

{-| -}

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Annotation exposing (Annotation)
import Elm.Case
import Elm.Case.Branch
import Gen.CodeGen.Generate as Generate exposing (Directory(..))
import Gen.Debug
import Gen.Maybe
import Gen.String
import Iso3166
import Json.Decode
import Json.Encode
import LanguageTag.Country as Country exposing (Country)
import LanguageTag.ExtendedLanguage as ExtendedLanguage
import LanguageTag.Language as Language
import LanguageTag.Parser
import LanguageTag.PrivateUse as PrivateUse
import LanguageTag.Script as Script exposing (Script)
import LanguageTag.Variant as Variant exposing (Variant)
import Set exposing (Set)
import String.Extra


main : Program Json.Encode.Value () ()
main =
    Generate.fromDirectory
        (\directory ->
            case getEnglishData directory of
                Err e ->
                    [ error "Error" e ]

                Ok ( languagesEnglishDict, territoriesEnglishDict ) ->
                    let
                        languageNames : Set String
                        languageNames =
                            Dict.values languagesEnglishDict
                                |> Set.fromList
                                |> Set.insert "Pidgin"
                                |> Set.insert "Gaelic"

                        shared =
                            { languageNames = languageNames
                            , languagesEnglishDict = languagesEnglishDict
                            , territoriesEnglishDict = territoriesEnglishDict
                            }

                        ( modulesStatus, languageFiles ) =
                            files directory shared
                    in
                    commonFiles directory shared modulesStatus
                        ++ languageFiles
        )


type alias Shared =
    { languageNames : Set String
    , languagesEnglishDict : Dict String String
    , territoriesEnglishDict : Dict String String
    }


type alias Locale =
    { key : String
    , fullName : String
    , moduleName : ModuleName
    }


type alias Language =
    { fullName : String
    , moduleName : ModuleName
    }


type alias ModuleName =
    List String


type alias ModuleStatus =
    { territories : DictStatus }


type DictStatus
    = Present
    | Absent { parent : ModuleName }


commonFiles : Directory -> Shared -> Dict ModuleName ModuleStatus -> List Elm.File
commonFiles (Directory directory) shared modulesStatus =
    let
        allLocales : List Locale
        allLocales =
            directory.directories
                |> Dict.keys
                |> List.filterMap
                    (\key ->
                        parseLanguageTag shared key
                            |> Maybe.map
                                (\{ fullName, moduleName } ->
                                    { key = key
                                    , fullName = fullName
                                    , moduleName = moduleName
                                    }
                                )
                    )
    in
    [ localizedFile allLocales modulesStatus
    , mainFile allLocales
    ]


mainFile : List Locale -> Elm.File
mainFile allLocales =
    Elm.file [ "Cldr" ]
        [ countryCodeTypeDeclaration
        , allLocalesDeclaration allLocales
        , localeToEnglishNameDeclaration allLocales
        , toAlpha2Declaration
        , fromAlpha2Declaration
        , allCountryCodesDeclaration
        ]


countryCodeTypeDeclaration : Elm.Declaration
countryCodeTypeDeclaration =
    allCountryCodes
        |> List.map Elm.variant
        |> Elm.customType "CountryCode"
        |> Elm.withDocumentation "All the supported country codes. `GT` and `LT` are defined in Basics so we define them as `GT_` and `LT_`."
        |> Elm.exposeWith { exposeConstructor = True, group = Nothing }


allCountryCodesDeclaration : Elm.Declaration
allCountryCodesDeclaration =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.named [] "CountryCode"
    in
    allCountryCodes
        |> List.map Elm.val
        |> Elm.list
        |> Elm.withType (Annotation.list countryCodeAnnotation)
        |> Elm.declaration "allCountryCodes"
        |> Elm.withDocumentation "All `CountryCode`s sorted alphabetically."
        |> Elm.expose


allLocalesDeclaration : List Locale -> Elm.Declaration
allLocalesDeclaration allLocales =
    allLocales
        |> List.map (\{ key } -> Elm.string key)
        |> Elm.list
        |> Elm.declaration "allLocales"
        |> Elm.withDocumentation "All the supported locales."
        |> Elm.expose


fromAlpha2Declaration : Elm.Declaration
fromAlpha2Declaration =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.named [] "CountryCode"

        implementation : Elm.Expression -> Elm.Expression
        implementation countryCodeExpr =
            Elm.Case.string countryCodeExpr
                { cases =
                    allCountryCodes
                        |> List.map
                            (\countryCode ->
                                ( countryCode
                                , Elm.val countryCode
                                    |> Gen.Maybe.make_.just
                                )
                            )
                , otherwise = Gen.Maybe.make_.nothing
                }
                |> Elm.withType (Annotation.maybe countryCodeAnnotation)
    in
    implementation
        |> Elm.fn ( "countryCode", Just Annotation.string )
        |> Elm.declaration "fromAlpha2"
        |> Elm.withDocumentation "`CountryCode` from two-letter `ISO 3166-1 alpha-2`."
        |> Elm.expose


toAlpha2Declaration : Elm.Declaration
toAlpha2Declaration =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.named [] "CountryCode"

        implementation : Elm.Expression -> Elm.Expression
        implementation countryCodeExpr =
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
                |> Elm.Case.custom countryCodeExpr countryCodeAnnotation
    in
    implementation
        |> Elm.fn ( "countryCode", Just countryCodeAnnotation )
        |> Elm.declaration "toAlpha2"
        |> Elm.withDocumentation "Two-letter `ISO 3166-1 alpha-2` code from `CountryCode`."
        |> Elm.expose


localeToEnglishNameDeclaration : List Locale -> Elm.Declaration
localeToEnglishNameDeclaration allLocales =
    let
        implementation : Elm.Expression -> Elm.Expression
        implementation locale =
            caseOnLocale allLocales
                locale
                { case_ = \{ fullName } -> Gen.Maybe.make_.just <| Elm.string fullName
                , otherwise = Gen.Maybe.make_.nothing
                }
    in
    implementation
        |> Elm.fn ( "locale", Just Annotation.string )
        |> Elm.withType
            (Annotation.function
                [ Annotation.string ]
                (Annotation.maybe Annotation.string)
            )
        |> Elm.declaration "localeToEnglishName"
        |> Elm.withDocumentation "Get the english name of a locale."
        |> Elm.expose


{-| Split the input on dashes and pattern match with the longest prefix we know of.
-}
caseOnLocale :
    List Locale
    -> Elm.Expression
    ->
        { case_ : Locale -> Elm.Expression
        , otherwise : Elm.Expression
        }
    -> Elm.Expression
caseOnLocale allLocales input { case_, otherwise } =
    allLocales
        |> List.map
            (\locale ->
                let
                    splat : List String
                    splat =
                        String.split "-" locale.key
                in
                ( splat, locale )
            )
        |> List.sortWith
            (\( l, _ ) ( r, _ ) -> sortSplitLocale l r)
        |> List.map
            (\( splat, locale ) ->
                Elm.Case.Branch.listWithRemaining
                    { patterns = List.map (\s -> Elm.Case.Branch.string s ()) splat
                    , gather = \i _ -> i
                    , startWith = ()
                    , finally = \_ _ -> case_ locale
                    , remaining = Elm.Case.Branch.ignore ()
                    }
            )
        |> (\cases ->
                cases ++ [ Elm.Case.Branch.ignore otherwise ]
           )
        |> Elm.Case.custom
            (Gen.String.call_.split (Elm.string "-") input)
            (Annotation.list Annotation.string)


sortSplitLocale : List comparable -> List comparable -> Order
sortSplitLocale l r =
    -- We want to sort alphabetically, but have longer lists first so we can match from most specific to least specific
    case ( l, r ) of
        ( [], [] ) ->
            EQ

        ( [], _ :: _ ) ->
            GT

        ( _ :: _, [] ) ->
            LT

        ( lh :: lt, rh :: rt ) ->
            let
                cmp : Order
                cmp =
                    compare lh rh
            in
            if cmp == EQ then
                sortSplitLocale lt rt

            else
                cmp


localizedFile : List Locale -> Dict ModuleName ModuleStatus -> Elm.File
localizedFile allLocales modulesStatus =
    Elm.file [ "Cldr", "Localized" ]
        [ localizedCountryCodeToNameDeclaration allLocales modulesStatus
        ]


localizedCountryCodeToNameDeclaration : List Locale -> Dict ModuleName ModuleStatus -> Elm.Declaration
localizedCountryCodeToNameDeclaration allLocales modulesStatus =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.named [ "Cldr" ] "CountryCode"

        functionAnnotation : Annotation
        functionAnnotation =
            Annotation.function
                [ countryCodeAnnotation ]
                Annotation.string

        implementation : Elm.Expression -> Elm.Expression -> Elm.Expression
        implementation locale countryCode =
            caseOnLocale allLocales
                locale
                { case_ =
                    \{ moduleName } ->
                        let
                            go : ModuleName -> Elm.Expression
                            go name =
                                case Dict.get name modulesStatus of
                                    Just { territories } ->
                                        case territories of
                                            Present ->
                                                Elm.apply
                                                    (Elm.value
                                                        { importFrom = "Cldr" :: moduleName
                                                        , name = "countryCodeToName"
                                                        , annotation = Just functionAnnotation
                                                        }
                                                    )
                                                    [ countryCode ]
                                                    |> Gen.Maybe.make_.just

                                            Absent { parent } ->
                                                go parent

                                    Nothing ->
                                        Gen.Debug.todo "Could not find whether the module was generated or not"
                        in
                        go moduleName
                , otherwise = Gen.Maybe.make_.nothing
                }
    in
    implementation
        |> Elm.fn2
            ( "locale", Just Annotation.string )
            ( "countryCode", Just countryCodeAnnotation )
        |> Elm.withType
            (Annotation.function
                [ Annotation.string, countryCodeAnnotation ]
                (Annotation.maybe Annotation.string)
            )
        |> Elm.declaration "countryCodeToName"
        |> Elm.expose


files :
    Directory
    -> Shared
    ->
        ( Dict ModuleName ModuleStatus
        , List Elm.File
        )
files ((Directory dir) as directory) ({ languagesEnglishDict } as shared) =
    let
        ( allDictionaries, allErrors ) =
            dir.directories
                |> Dict.keys
                |> List.foldl tryAddDictionary ( Dict.empty, [] )

        ( allFiles, modulesStatus ) =
            allDictionaries
                |> Dict.toList
                |> List.foldl
                    (\( moduleName, { fullName, territories } ) ( filesAcc, dictAcc ) ->
                        let
                            parentModule : ModuleName
                            parentModule =
                                getParentModule territories moduleName

                            parent :
                                { fullName : String
                                , territories : Dict String String
                                }
                            parent =
                                allDictionaries
                                    |> Dict.get parentModule
                                    |> Maybe.withDefault
                                        { fullName = ""
                                        , territories = Dict.empty
                                        }
                        in
                        case
                            countryCodeToNameDeclaration
                                { fullName = parent.fullName
                                , moduleName = parentModule
                                , territories = parent.territories
                                }
                                fullName
                                territories
                        of
                            Just declaration ->
                                ( Elm.file ("Cldr" :: moduleName)
                                    [ declaration
                                    ]
                                    :: filesAcc
                                , Dict.insert moduleName { territories = Present } dictAcc
                                )

                            Nothing ->
                                ( filesAcc
                                , Dict.insert moduleName
                                    { territories =
                                        Absent
                                            { parent = parentModule
                                            }
                                    }
                                    dictAcc
                                )
                    )
                    ( [], Dict.empty )

        tryAddDictionary :
            String
            ->
                ( Dict ModuleName { fullName : String, territories : Dict String String }
                , List Elm.File
                )
            ->
                ( Dict ModuleName { fullName : String, territories : Dict String String }
                , List Elm.File
                )
        tryAddDictionary key ( dictAcc, errAcc ) =
            case parseLanguageTag shared key of
                Just { fullName, moduleName } ->
                    case getTerritories key directory of
                        Ok territories ->
                            ( Dict.insert moduleName
                                { fullName = fullName
                                , territories = territories
                                }
                                dictAcc
                            , errAcc
                            )

                        Err e ->
                            ( dictAcc, error fullName e :: errAcc )

                Nothing ->
                    if key == "und" then
                        -- Unknown language
                        ( dictAcc, errAcc )

                    else
                        let
                            name : String
                            name =
                                Dict.get key languagesEnglishDict
                                    |> Maybe.withDefault "Nothing"
                        in
                        ( dictAcc
                        , error key
                            ("Failed to parse language tag, language name is " ++ name)
                            :: errAcc
                        )
    in
    ( modulesStatus, allErrors ++ allFiles )


getParentModule : Dict String String -> ModuleName -> ModuleName
getParentModule territories moduleName =
    case moduleName of
        [ "Spanish", "Argentina" ] ->
            [ "Spanish" ]

        [ "Spanish", _ ] ->
            [ "Spanish", "Argentina" ]

        [ "English", "UnitedKingdom" ] ->
            [ "English" ]

        [ "English", _ ] ->
            if Dict.get "MF" territories == Just "St. Martin" then
                [ "English" ]

            else
                [ "English", "UnitedKingdom" ]

        [ "Portuguese", "Portugal" ] ->
            [ "Portuguese" ]

        [ "Portuguese", _ ] ->
            [ "Portuguese", "Portugal" ]

        _ ->
            moduleName
                |> List.reverse
                |> List.drop 1
                |> List.reverse


parseLanguageTag : Shared -> String -> Maybe Language
parseLanguageTag { languageNames, languagesEnglishDict, territoriesEnglishDict } key =
    if key == "und" then
        Nothing

    else
        let
            traverse : (a -> Result String b) -> Maybe a -> Result String (Maybe b)
            traverse f v =
                case v of
                    Nothing ->
                        Ok Nothing

                    Just w ->
                        Result.map Just (f w)

            parsed : Result String Language
            parsed =
                case LanguageTag.Parser.parseBcp47 key of
                    Just ( language, { script, region, variants, extensions, privateUse } ) ->
                        if not (List.isEmpty extensions) then
                            Err <| "Unsupported! extension = " ++ String.join ", " (List.map ExtendedLanguage.toCodeString extensions)

                        else
                            case privateUse of
                                Just privateUseParts ->
                                    Err <| "Unsupported! privateUse = " ++ PrivateUse.toCodeString privateUseParts

                                Nothing ->
                                    Result.map4
                                        (\( languageName, splitLanguageName ) scriptName regionName variantName ->
                                            createLanguage
                                                { languageName = languageName
                                                , splitLanguageName = splitLanguageName
                                                , scriptName = scriptName
                                                , regionName = regionName
                                                , variantName = variantName
                                                }
                                        )
                                        (languageString language)
                                        (traverse scriptToString script)
                                        (traverse (regionToString territoriesEnglishDict) region)
                                        (variantsToString variants)

                    Nothing ->
                        Err <| "Could not parse BCP 47 tag: " ++ key

            languageString : Language.Language -> Result String ( String, List String )
            languageString language =
                let
                    languageCode : String
                    languageCode =
                        Language.toCodeString language
                in
                case Dict.get languageCode languagesEnglishDict of
                    Nothing ->
                        Err <| "Language not found: " ++ languageCode

                    Just languageName ->
                        case splitLanguage languageNames languageName of
                            Nothing ->
                                Err <| "Failed to split language name: " ++ languageName

                            Just splat ->
                                Ok ( languageName, splat )
        in
        Result.toMaybe parsed


createLanguage :
    { a
        | languageName : String
        , splitLanguageName : List String
        , scriptName : Maybe String
        , regionName : Maybe String
        , variantName : Maybe String
    }
    -> Language
createLanguage data =
    { fullName = fullLanguageName data
    , moduleName = toModuleName data
    }


fullLanguageName :
    { a
        | languageName : String
        , scriptName : Maybe String
        , regionName : Maybe String
        , variantName : Maybe String
    }
    -> String
fullLanguageName { languageName, scriptName, regionName, variantName } =
    let
        wrapString : String -> Maybe String -> String -> String
        wrapString before value after =
            case value of
                Nothing ->
                    ""

                Just w ->
                    before ++ w ++ after
    in
    languageName
        ++ wrapString " (" scriptName ")"
        ++ wrapString " - " regionName ""
        ++ wrapString " (" variantName ")"


toModuleName :
    { a
        | splitLanguageName : List String
        , scriptName : Maybe String
        , regionName : Maybe String
        , variantName : Maybe String
    }
    -> ModuleName
toModuleName { splitLanguageName, scriptName, regionName, variantName } =
    (splitLanguageName
        ++ List.filterMap identity
            [ scriptName
            , regionName
            , variantName
            ]
    )
        |> List.map
            (\name ->
                name
                    |> cleanName
                    |> String.replace " " ""
            )


variantsToString : List Variant -> Result String (Maybe String)
variantsToString variants =
    case List.map Variant.toCodeString variants of
        [] ->
            Ok Nothing

        [ "polyton" ] ->
            Ok (Just "Polytonic")

        [ "valencia" ] ->
            Ok (Just "Valencia")

        [ "tarask" ] ->
            Ok (Just "Taraškievica")

        strings ->
            Err <| "Unsupported! variants = " ++ String.join ", " strings


regionToString : Dict String String -> Country -> Result String String
regionToString territoriesEnglishDict region =
    let
        regionCode : String
        regionCode =
            Country.toCodeString region
    in
    case Dict.get regionCode territoriesEnglishDict of
        Nothing ->
            Err <| "Could not find territory: " ++ regionCode

        Just territoryName ->
            Ok territoryName


scriptToString : Script -> Result String String
scriptToString script =
    case Script.toCodeString script of
        "Hans" ->
            Ok "Simplified"

        "Hant" ->
            Ok "Traditional"

        "Latn" ->
            Ok "Latin"

        "Cyrl" ->
            Ok "Cyrillic"

        "Arab" ->
            Ok "Arabic"

        "Guru" ->
            Ok "Gurmukhi"

        scriptString ->
            Err <| "Unsupported! script = " ++ scriptString


getEnglishData : Directory -> Result String ( Dict String String, Dict String String )
getEnglishData directory =
    Result.map2 Tuple.pair
        (getLanguages "en" directory)
        (getTerritories "en" directory)


getTerritories : String -> Directory -> Result String (Dict String String)
getTerritories key (Directory directory) =
    case Dict.get key directory.directories of
        Just (Directory subdirectory) ->
            case Dict.get "territories.json" subdirectory.files of
                Just territoriesJson ->
                    decodeTerritories key territoriesJson
                        |> Result.mapError (\e -> "\"territories.json\": decoding failed: " ++ Json.Decode.errorToString e)

                Nothing ->
                    Err "Could not find \"territories.json\""

        Nothing ->
            Err <| "Could not find directory \"" ++ key ++ "\""


getLanguages : String -> Directory -> Result String (Dict String String)
getLanguages key (Directory directory) =
    case Dict.get key directory.directories of
        Just (Directory subdirectory) ->
            case Dict.get "languages.json" subdirectory.files of
                Just languagesJson ->
                    Result.mapError (\e -> "\"languages.json\": decoding failed: " ++ Json.Decode.errorToString e) <| decodeLanguages languagesJson

                Nothing ->
                    Err "Could not find \"languages.json\""

        Nothing ->
            Err <| "Could not find directory \"" ++ key ++ "\""


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

        replaceWithVariant : String -> Dict String String -> Dict String String
        replaceWithVariant k =
            replace k (k ++ "-alt-variant")

        replaceWithShort : String -> Dict String String -> Dict String String
        replaceWithShort k =
            replace k (k ++ "-alt-short")

        replace : String -> String -> Dict String String -> Dict String String
        replace to from dict =
            case Dict.get from dict of
                Nothing ->
                    dict

                Just v ->
                    Dict.insert to v dict
    in
    Json.Decode.decodeString decoder input
        |> Result.map
            (\dict ->
                dict
                    |> replaceWithVariant "CD"
                    |> replaceWithVariant "CG"
                    |> replaceWithVariant "CZ"
                    |> replaceWithShort "HK"
                    |> replaceWithShort "MO"
                    |> replaceWithShort "PS"
                    |> replaceWithVariant "TL"
            )


allCountryCodes : List String
allCountryCodes =
    Iso3166.all
        |> List.map (Iso3166.toAlpha2 >> toUpper)
        -- Kosovo
        |> (::) "XK"
        |> List.sort


toUpper : String -> String
toUpper input =
    input
        |> String.toUpper
        |> String.replace "GT" "GT_"
        |> String.replace "LT" "LT_"


{-| Returns nothing if it's identical to the parent language.
-}
countryCodeToNameDeclaration :
    { fullName : String
    , moduleName : ModuleName
    , territories : Dict String String
    }
    -> String
    -> Dict String String
    -> Maybe Elm.Declaration
countryCodeToNameDeclaration parent languageName territories =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.namedWith [ "Cldr" ] "CountryCode" []

        parentFunction : Elm.Expression
        parentFunction =
            Elm.value
                { importFrom = "Cldr" :: parent.moduleName
                , name = "countryCodeToName"
                , annotation = Just <| Annotation.function [ countryCodeAnnotation ] Annotation.string
                }

        branches : List Elm.Case.Branch
        branches =
            allCountryCodes
                |> List.filterMap
                    (\countryCode ->
                        let
                            countryCodeClean : String
                            countryCodeClean =
                                String.replace "_" "" countryCode
                        in
                        Dict.get countryCodeClean territories
                            |> Maybe.andThen
                                (\name ->
                                    if Just name == Dict.get countryCodeClean parent.territories then
                                        Nothing

                                    else
                                        Just <| Elm.Case.branch0 countryCode (Elm.string name)
                                )
                    )

        table : String
        table =
            allCountryCodes
                |> List.filterMap
                    (\countryCode ->
                        let
                            countryCodeClean : String
                            countryCodeClean =
                                String.replace "_" "" countryCode
                        in
                        Dict.get countryCodeClean territories
                            |> Maybe.map (\name -> "    " ++ countryCodeClean ++ " " ++ name)
                    )
                |> String.join "\n"
    in
    if List.isEmpty branches then
        -- parentFunction
        --     |> Elm.declaration "countryCodeToName"
        --     |> Elm.withDocumentation ("Name for `CountryCode` in " ++ languageName ++ ".\n\nThis is identical to the " ++ parent.fullName ++ " version.\n\n" ++ table)
        --     |> Elm.expose
        Nothing

    else
        Elm.fn ( "countryCode", Just countryCodeAnnotation )
            (\countryCodeExpr ->
                (if List.length branches == List.length allCountryCodes then
                    branches

                 else
                    branches
                        ++ [ Elm.Case.Branch.ignore
                                (Elm.apply parentFunction [ countryCodeExpr ])
                           ]
                )
                    |> Elm.Case.custom countryCodeExpr countryCodeAnnotation
            )
            |> Elm.declaration "countryCodeToName"
            |> Elm.withDocumentation ("Name for `CountryCode` in " ++ languageName ++ ".\n\n" ++ table)
            |> Elm.expose
            |> Just


cleanName : String -> String
cleanName name =
    name
        |> String.replace "." ""
        |> String.replace "&" "And"
        |> String.replace "-" ""
        |> String.replace "’" ""
        |> String.replace "(" ""
        |> String.replace ")" ""
        |> String.Extra.toSentenceCase


splitLanguage : Set String -> String -> Maybe (List String)
splitLanguage languagesNames lang =
    case
        cleanName lang
            |> String.split " "
            |> List.reverse
    of
        [ atom ] ->
            Just [ atom ]

        [ "(Latin)", "Hindi" ] ->
            Just [ "Hindi", "Latin" ]

        [ prefix, suffix ] ->
            if Set.member suffix languagesNames then
                Just [ suffix, prefix ]

            else if Set.member prefix languagesNames then
                Just [ prefix, suffix ]

            else
                Nothing

        head :: tail ->
            if Set.member head languagesNames then
                Just [ head, String.concat <| List.reverse tail ]

            else
                Nothing

        _ ->
            Nothing


error : String -> String -> Elm.File
error file msg =
    Elm.file [ file ]
        [ Elm.declaration "error" <|
            Elm.string msg
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
