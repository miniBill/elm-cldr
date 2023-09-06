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
import LanguageTag
import LanguageTag.ExtendedLanguage as ExtendedLanguage
import LanguageTag.Language as Language
import LanguageTag.Parser
import LanguageTag.PrivateUse as PrivateUse
import LanguageTag.Region as Region exposing (Region)
import LanguageTag.Script as Script exposing (Script)
import LanguageTag.Variant as Variant exposing (Variant)
import List.Extra
import Set exposing (Set)
import String.Extra


main : Program Json.Encode.Value () ()
main =
    Generate.fromDirectory
        (\directory ->
            case getLocaleData "en" directory of
                Err e ->
                    [ error "Error" e ]

                Ok english ->
                    let
                        shared : Shared
                        shared =
                            { english = english
                            , allLocales = allLocales
                            }

                        { modulesStatus, languageFiles, allLocales } =
                            files directory english
                    in
                    commonFiles directory shared modulesStatus
                        ++ languageFiles
        )


type alias Shared =
    { english : LocaleData
    , allLocales : Dict String LocaleData
    }


type alias LocaleData =
    { key : String
    , languages : Dict String String
    , languageNames : Set String
    , territories : Dict String String
    , scripts : Dict String String
    , variants : Dict String String
    }


type alias Locale =
    { key : String
    , fullEnglishName : String
    , fullNativeName : String
    , moduleName : ModuleName
    }


type alias Language =
    { fullEnglishName : String
    , fullNativeName : String
    , moduleName : ModuleName
    }


type alias ModuleName =
    List String


type alias ModuleStatus =
    { territories : DictStatus }


type DictStatus
    = Present
      -- Pointer to the parent ModuleName
    | Absent ModuleName


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
                            |> Result.toMaybe
                            |> Maybe.map
                                (\{ fullEnglishName, fullNativeName, moduleName } ->
                                    { key = key
                                    , fullEnglishName = fullEnglishName
                                    , fullNativeName = fullNativeName
                                    , moduleName = moduleName
                                    }
                                )
                    )

        defaultContent : Maybe (List String)
        defaultContent =
            Dict.get "defaultContent.json" directory.files
                |> Maybe.andThen
                    (\json ->
                        json
                            |> Json.Decode.decodeString defaultContentDecoder
                            |> Result.toMaybe
                    )

        likelySubtags : Maybe (Dict String String)
        likelySubtags =
            Dict.get "likelySubtags.json" directory.files
                |> Maybe.andThen
                    (\json ->
                        json
                            |> Json.Decode.decodeString likelySubtagsDecoder
                            |> Result.toMaybe
                    )

        defaultContentDecoder : Json.Decode.Decoder (List String)
        defaultContentDecoder =
            Json.Decode.at [ "defaultContent" ]
                (Json.Decode.list Json.Decode.string)

        likelySubtagsDecoder : Json.Decode.Decoder (Dict String String)
        likelySubtagsDecoder =
            Json.Decode.at [ "supplemental", "likelySubtags" ]
                (Json.Decode.dict Json.Decode.string)
    in
    [ localizedFile allLocales modulesStatus
    , mainFile allLocales { defaultContent = defaultContent, likelySubtags = likelySubtags } modulesStatus
    ]


mainFile : List Locale -> { defaultContent : Maybe (List String), likelySubtags : Maybe (Dict String String) } -> Dict ModuleName ModuleStatus -> Elm.File
mainFile allLocales { defaultContent, likelySubtags } modulesStatus =
    Elm.file [ "Cldr" ]
        [ countryCodeTypeDeclaration
        , allLocalesDeclaration allLocales
        , allNontrivialLocalesDeclaration allLocales modulesStatus
        , localeToEnglishNameDeclaration allLocales
        , localeToNativeNameDeclaration allLocales
        , toAlpha2Declaration
        , fromAlpha2Declaration
        , allCountryCodesDeclaration
        , likelySubtagsDeclaration allLocales defaultContent likelySubtags
        ]


likelySubtagsDeclaration : List Locale -> Maybe (List String) -> Maybe (Dict String String) -> Elm.Declaration
likelySubtagsDeclaration allLocales defaultContentMaybe likelySubtagsMaybe =
    let
        implementation : Elm.Expression -> Elm.Expression
        implementation locale =
            case ( likelySubtagsMaybe, defaultContentMaybe ) of
                ( Nothing, _ ) ->
                    Gen.Debug.todo "Could not parse likelySubtags.json"

                ( _, Nothing ) ->
                    Gen.Debug.todo "Could not parse defaultContent.json"

                ( Just likelySubtags, Just defaultContent ) ->
                    Elm.Case.string locale
                        { cases =
                            allLocales
                                |> List.filterMap
                                    (\{ key } ->
                                        let
                                            fromLikely () =
                                                Dict.get key likelySubtags
                                                    |> Maybe.map
                                                        (\likelySubtag ->
                                                            ( key
                                                            , Gen.Maybe.make_.just <| Elm.string likelySubtag
                                                            )
                                                        )
                                        in
                                        case
                                            List.filter
                                                (\line -> String.startsWith (key ++ "-") line)
                                                defaultContent
                                        of
                                            [] ->
                                                fromLikely ()

                                            [ likelySubtag ] ->
                                                ( key
                                                , Gen.Maybe.make_.just <| Elm.string likelySubtag
                                                )
                                                    |> Just

                                            _ ->
                                                fromLikely ()
                                    )
                        , otherwise = Gen.Maybe.make_.nothing
                        }
                        |> Elm.withType (Gen.Maybe.annotation_.maybe Annotation.string)
    in
    implementation
        |> Elm.fn ( "locale", Just Annotation.string )
        |> Elm.declaration "likelySubtags"
        |> Elm.expose


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


allNontrivialLocalesDeclaration : List Locale -> Dict ModuleName ModuleStatus -> Elm.Declaration
allNontrivialLocalesDeclaration allLocales modulesStatus =
    allLocales
        |> List.filterMap
            (\{ key, moduleName } ->
                case Dict.get moduleName modulesStatus of
                    Just { territories } ->
                        case territories of
                            Present ->
                                Just <| Elm.string key

                            Absent _ ->
                                Nothing

                    Nothing ->
                        Just <| Gen.Debug.todo <| "Could not find info about locale " ++ key
            )
        |> Elm.list
        |> Elm.declaration "allNontrivialLocales"
        |> Elm.withDocumentation "All the locales that are not identical to some parent locale."
        |> Elm.expose


fromAlpha2Declaration : Elm.Declaration
fromAlpha2Declaration =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.named [] "CountryCode"

        implementation : Elm.Expression -> Elm.Expression
        implementation countryCodeExpr =
            Elm.Case.string (Gen.String.call_.toLower countryCodeExpr)
                { cases =
                    allCountryCodes
                        |> List.map
                            (\countryCode ->
                                ( countryCode
                                    |> String.replace "_" ""
                                    |> String.toLower
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
                { case_ = \{ fullEnglishName } -> Just <| Gen.Maybe.make_.just <| Elm.string fullEnglishName
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


localeToNativeNameDeclaration : List Locale -> Elm.Declaration
localeToNativeNameDeclaration allLocales =
    let
        implementation : Elm.Expression -> Elm.Expression
        implementation locale =
            caseOnLocale allLocales
                locale
                { case_ =
                    \{ fullNativeName } ->
                        if String.isEmpty fullNativeName then
                            Nothing

                        else
                            Just <| Gen.Maybe.make_.just <| Elm.string fullNativeName
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
        |> Elm.declaration "localeToNativeName"
        |> Elm.withDocumentation "Get the english name of a locale."
        |> Elm.expose


{-| Split the input on dashes and pattern match with the longest prefix we know of.
-}
caseOnLocale :
    List Locale
    -> Elm.Expression
    ->
        { case_ : Locale -> Maybe Elm.Expression
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
        |> List.filterMap
            (\( splat, locale ) ->
                case_ locale
                    |> Maybe.map
                        (\expr ->
                            Elm.Case.Branch.listWithRemaining
                                { patterns = List.map (\s -> Elm.Case.Branch.string s ()) splat
                                , gather = \i _ -> i
                                , startWith = ()
                                , finally = \_ _ -> expr
                                , remaining = Elm.Case.Branch.ignore ()
                                }
                        )
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
                            go : Bool -> ModuleName -> Maybe Elm.Expression
                            go first name =
                                case Dict.get name modulesStatus of
                                    Just { territories } ->
                                        case territories of
                                            Present ->
                                                Elm.apply
                                                    (Elm.value
                                                        { importFrom = "Cldr" :: name
                                                        , name = "countryCodeToName"
                                                        , annotation = Just functionAnnotation
                                                        }
                                                    )
                                                    [ countryCode ]
                                                    |> Gen.Maybe.make_.just
                                                    |> Just

                                            Absent parent ->
                                                if first && List.Extra.isPrefixOf parent name then
                                                    Nothing

                                                else
                                                    go False parent

                                    Nothing ->
                                        Gen.Debug.todo "Could not find whether the module was generated or not"
                                            |> Just
                        in
                        go True moduleName
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
    -> LocaleData
    ->
        { modulesStatus : Dict ModuleName ModuleStatus
        , languageFiles : List Elm.File
        , allLocales : Dict String LocaleData
        }
files ((Directory dir) as directory) english =
    let
        { allDictionaries, allErrors } =
            dir.directories
                |> Dict.keys
                |> List.foldl tryAddDictionary { allDictionaries = Dict.empty, allErrors = [] }

        { allFiles, modulesStatus, allLocales } =
            allDictionaries
                |> Dict.toList
                |> List.sortBy
                    (\( moduleName, { data } ) ->
                        ( List.length moduleName
                        , List.length (getParentModule data.territories moduleName)
                        )
                    )
                |> List.foldl
                    (\( moduleName, { fullEnglishName, data } ) acc ->
                        let
                            parentModuleName : ModuleName
                            parentModuleName =
                                getParentModule data.territories moduleName

                            parent :
                                Maybe
                                    { fullEnglishName : String
                                    , data : LocaleData
                                    }
                            parent =
                                Dict.get parentModuleName allDictionaries
                        in
                        case
                            countryCodeToNameDeclaration
                                { parentModuleName = parentModuleName }
                                parent
                                { fullEnglishName = fullEnglishName
                                , territories = data.territories
                                }
                                acc.modulesStatus
                        of
                            Just declaration ->
                                { acc
                                    | allFiles =
                                        Elm.file ("Cldr" :: moduleName)
                                            [ declaration
                                            ]
                                            :: acc.allFiles
                                    , modulesStatus =
                                        Dict.insert moduleName
                                            { territories = Present }
                                            acc.modulesStatus
                                    , allLocales = Dict.insert data.key data acc.allLocales
                                }

                            Nothing ->
                                { acc
                                    | modulesStatus =
                                        Dict.insert moduleName
                                            { territories = Absent parentModuleName }
                                            acc.modulesStatus
                                    , allLocales = Dict.insert data.key data acc.allLocales
                                }
                    )
                    { allFiles = [], modulesStatus = Dict.empty, allLocales = Dict.empty }

        tryAddDictionary :
            String
            ->
                { allDictionaries : Dict ModuleName { fullEnglishName : String, data : LocaleData }
                , allErrors : List Elm.File
                }
            ->
                { allDictionaries : Dict ModuleName { fullEnglishName : String, data : LocaleData }
                , allErrors : List Elm.File
                }
        tryAddDictionary key acc =
            case parseLanguageTag { english = english, allLocales = Dict.empty } key of
                Ok { fullEnglishName, moduleName } ->
                    case getLocaleData key directory of
                        Ok data ->
                            { acc
                                | allDictionaries =
                                    Dict.insert moduleName
                                        { fullEnglishName = fullEnglishName
                                        , data = data
                                        }
                                        acc.allDictionaries
                            }

                        Err err ->
                            { acc | allErrors = error fullEnglishName err :: acc.allErrors }

                Err err ->
                    if key == "und" then
                        -- Unknown language
                        acc

                    else
                        let
                            name : String
                            name =
                                Dict.get key english.languages
                                    |> Maybe.withDefault ("key - " ++ key)

                            msg : String
                            msg =
                                "Failed to parse language tag, language name is " ++ name ++ ", error is: " ++ err
                        in
                        { acc | allErrors = error key msg :: acc.allErrors }
    in
    { modulesStatus = modulesStatus
    , languageFiles = allErrors ++ allFiles
    , allLocales = allLocales
    }


getParentModule : Dict String String -> ModuleName -> ModuleName
getParentModule territories moduleName =
    case moduleName of
        [ "Spanish", region ] ->
            let
                likeBrazil : List String
                likeBrazil =
                    [ "Belize"
                    , "Cuba"
                    , "LatinAmerica"
                    , "Uruguay"
                    ]

                likeArgentina : List String
                likeArgentina =
                    [ "Bolivia"
                    , "Brazil"
                    , "Chile"
                    , "Colombia"
                    , "CostaRica"
                    , "DominicanRepublic"
                    , "Ecuador"
                    , "ElSalvador"
                    , "Guatemala"
                    , "Honduras"
                    , "Mexico"
                    , "Nicaragua"
                    , "Panama"
                    , "Paraguay"
                    , "Peru"
                    , "PuertoRico"
                    , "UnitedStates"
                    , "Venezuela"
                    ]
            in
            if region == "ElSalvador" then
                [ "Spanish", "PuertoRico" ]

            else if List.member region likeBrazil then
                [ "Spanish", "Brazil" ]

            else if List.member region likeArgentina then
                [ "Spanish", "Argentina" ]

            else
                [ "Spanish" ]

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


parseLanguageTag :
    Shared
    -> String
    -> Result String Language
parseLanguageTag { english, allLocales } key =
    if key == "und" then
        Err "Undefined language"

    else
        let
            traverse : (a -> Result String b) -> Maybe a -> Result String (Maybe b)
            traverse f v =
                case v of
                    Nothing ->
                        Ok Nothing

                    Just w ->
                        Result.map Just (f w)

            getData :
                Language.Language
                -> LanguageTag.Options
                -> LocaleData
                ->
                    Result
                        String
                        { languageName : String
                        , splitLanguageName : List String
                        , scriptName : Maybe String
                        , regionName : Maybe String
                        , variantName : Maybe String
                        }
            getData language options localeData =
                Result.map4
                    (\( languageName, splitLanguageName ) scriptName regionName variantName ->
                        { languageName = languageName
                        , splitLanguageName = splitLanguageName
                        , scriptName = scriptName
                        , regionName = regionName
                        , variantName = variantName
                        }
                    )
                    (languageToString localeData language)
                    (traverse (scriptToString localeData) options.script)
                    (traverse (regionToString localeData) options.region)
                    (variantsToString localeData options.variants)

            languageToString : LocaleData -> Language.Language -> Result String ( String, List String )
            languageToString localeData language =
                let
                    languageCode : String
                    languageCode =
                        Language.toCodeString language
                in
                case Dict.get languageCode localeData.languages of
                    Nothing ->
                        Err <| "Language not found: " ++ languageCode

                    Just languageName ->
                        case splitLanguage localeData languageName of
                            Nothing ->
                                Err <| "Failed to split language name: " ++ languageName

                            Just splat ->
                                Ok ( languageName, splat )
        in
        case LanguageTag.Parser.parseBcp47 key of
            Just ( language, options ) ->
                if not (List.isEmpty options.extensions) then
                    Err <| "Unsupported! extension = " ++ String.join ", " (List.map ExtendedLanguage.toCodeString options.extensions)

                else
                    case options.privateUse of
                        Just privateUseParts ->
                            Err <| "Unsupported! privateUse = " ++ PrivateUse.toCodeString privateUseParts

                        Nothing ->
                            Result.map2
                                (\englishData nativeData ->
                                    { fullEnglishName = fullLanguageName englishData
                                    , fullNativeName =
                                        Maybe.map fullLanguageName nativeData
                                            |> Maybe.withDefault ""
                                    , moduleName = toModuleName englishData
                                    }
                                )
                                (getData language options english)
                                (case Dict.get key allLocales of
                                    Nothing ->
                                        Ok Nothing

                                    Just localeData ->
                                        getData language options localeData
                                            |> Result.toMaybe
                                            |> Ok
                                )

            Nothing ->
                Err <| "Could not parse BCP 47 tag: " ++ key


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
                    |> String.replace "orthography" ""
                    |> String.replace " " ""
            )


variantsToString : LocaleData -> List Variant -> Result String (Maybe String)
variantsToString localeData variants =
    case variants of
        [ variant ] ->
            Result.map Just <| variantToString localeData variant

        [] ->
            Ok Nothing

        _ ->
            Err "Multiple variant are not supported"


variantToString : LocaleData -> Variant -> Result String String
variantToString localeData variant =
    let
        variantCode : String
        variantCode =
            Variant.toCodeString variant
    in
    case Dict.get variantCode localeData.variants of
        Nothing ->
            Err <|
                "Could not find variant: "
                    ++ variantCode
                    ++ " in locale "
                    ++ localeData.key
                    ++ ", available variants are [ "
                    ++ String.join ", " (Dict.keys localeData.variants)
                    ++ " ]"

        Just variantName ->
            Ok variantName


regionToString : LocaleData -> Region -> Result String String
regionToString localeData region =
    let
        regionCode : String
        regionCode =
            Region.toCodeString region
    in
    case Dict.get regionCode localeData.territories of
        Nothing ->
            Err <| "Could not find region: " ++ regionCode

        Just territoryName ->
            Ok territoryName


scriptToString : LocaleData -> Script -> Result String String
scriptToString localeData script =
    let
        scriptString : String
        scriptString =
            Script.toCodeString script
    in
    case Dict.get scriptString localeData.scripts of
        Nothing ->
            Err <| "Could not find script: " ++ scriptString

        Just name ->
            Ok name


getLocaleData : String -> Directory -> Result String LocaleData
getLocaleData key directory =
    Result.map4
        (\languages territories scripts variants ->
            { key = key
            , languages = languages
            , languageNames =
                if key == "en" then
                    languages
                        |> Dict.values
                        |> Set.fromList
                        |> Set.insert "Pidgin"
                        |> Set.insert "Gaelic"

                else
                    languages
                        |> Dict.values
                        |> Set.fromList
            , territories = territories
            , scripts = scripts
            , variants = variants
            }
        )
        (getFile "languages" key directory)
        (getTerritories key directory)
        (getFile "scripts" key directory)
        (getVariants key directory)


getTerritories : String -> Directory -> Result String (Dict String String)
getTerritories key directory =
    let
        fixup : Dict String String -> Dict String String
        fixup dict =
            dict
                |> replaceWithVariant "CD"
                |> replaceWithVariant "CG"
                |> replaceWithVariant "CZ"
                |> replaceWithShort "HK"
                |> replaceWithShort "MO"
                |> replaceWithShort "PS"
                |> replaceWithVariant "TL"

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
    getFile "territories" key directory
        |> Result.map fixup


getVariants : String -> Directory -> Result String (Dict String String)
getVariants key directory =
    let
        fixup : Dict String v -> Dict String v
        fixup dict =
            Dict.foldl
                (\k v acc ->
                    Dict.insert (String.toLower k) v acc
                )
                Dict.empty
                dict
    in
    getFile "variants" key directory
        |> Result.map fixup
        -- Some locales don't have a variants.json file
        |> Result.withDefault Dict.empty
        |> Ok


getFile : String -> String -> Directory -> Result String (Dict String String)
getFile fileName key (Directory directory) =
    case Dict.get key directory.directories of
        Just (Directory subdirectory) ->
            case Dict.get (fileName ++ ".json") subdirectory.files of
                Just json ->
                    Json.Decode.decodeString
                        (Json.Decode.at
                            [ "main"
                            , key
                            , "localeDisplayNames"
                            , fileName
                            ]
                            (Json.Decode.dict Json.Decode.string)
                        )
                        json
                        |> Result.mapError
                            (\e ->
                                "\"" ++ fileName ++ ".json\": decoding failed: " ++ Json.Decode.errorToString e
                            )

                Nothing ->
                    Err <| "Could not find \"" ++ fileName ++ ".json\""

        Nothing ->
            Err <| "Could not find directory \"" ++ key ++ "\""


allCountryCodes : List String
allCountryCodes =
    Iso3166.all
        |> List.map (Iso3166.toAlpha2 >> toVariantName)
        -- Kosovo
        |> (::) "XK"
        |> List.sort


toVariantName : String -> String
toVariantName input =
    input
        |> String.toUpper
        |> String.replace "GT" "GT_"
        |> String.replace "LT" "LT_"


{-| Returns nothing if it's identical to the parent language.
-}
countryCodeToNameDeclaration :
    { parentModuleName : ModuleName }
    ->
        Maybe
            { fullEnglishName : String
            , data : LocaleData
            }
    ->
        { fullEnglishName : String
        , territories : Dict String String
        }
    -> Dict ModuleName ModuleStatus
    -> Maybe Elm.Declaration
countryCodeToNameDeclaration { parentModuleName } parent { fullEnglishName, territories } modulesStatus =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.namedWith [ "Cldr" ] "CountryCode" []

        parentFunction : Elm.Expression
        parentFunction =
            let
                go name =
                    case Dict.get name modulesStatus of
                        Nothing ->
                            Gen.Debug.todo ("Could not find module info for " ++ String.join "." name)

                        Just status ->
                            case status.territories of
                                Present ->
                                    Elm.value
                                        { importFrom = "Cldr" :: name
                                        , name = "countryCodeToName"
                                        , annotation = Just <| Annotation.function [ countryCodeAnnotation ] Annotation.string
                                        }

                                Absent absent ->
                                    go absent
            in
            go parentModuleName

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
                                    let
                                        parentName =
                                            parent
                                                |> Maybe.map (\{ data } -> data.territories)
                                                |> Maybe.andThen (Dict.get countryCodeClean)
                                    in
                                    if Just name == parentName then
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
        --     |> Elm.withDocumentation ("Name for `CountryCode` in " ++ fullEnglishName ++ ".\n\nThis is identical to the " ++ parent.fullEnglishName ++ " version.\n\n" ++ table)
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
            |> Elm.withDocumentation ("Name for `CountryCode` in " ++ fullEnglishName ++ ".\n\n" ++ table)
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


splitLanguage : LocaleData -> String -> Maybe (List String)
splitLanguage localeData lang =
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
            if Set.member suffix localeData.languageNames then
                Just [ suffix, prefix ]

            else if Set.member prefix localeData.languageNames then
                Just [ prefix, suffix ]

            else
                Nothing

        head :: tail ->
            if Set.member head localeData.languageNames then
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
