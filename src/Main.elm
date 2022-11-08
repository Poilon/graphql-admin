module Main exposing (main, view)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http exposing (Error)
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import String.Case as String
import Url


graphQlVariables : Value
graphQlVariables =
    Encode.object [ ( "loggedUserId", Encode.string "0beff0c8-37ee-4650-aca2-65498c7f522a" ) ]


makeRequest : Cmd Msg
makeRequest =
    -- Http.post
    --     { url = "http://localhost:3000/graphql"
    --     , body =
    --         Http.stringBody "application/json" "test"
    --     , expect = Http.expectString GotResponse
    --     }
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , url = "http://localhost:3000/graphql"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "query", introspectionGraphQlQuery )
                    , ( "variables", graphQlVariables )
                    ]
                )
        , expect = Http.expectJson GotIntrospectionResponse introspectionDataDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type alias DataModel =
    { name : String, fields : Maybe (List Field) }


type alias Field =
    { name : String, type_ : Type }


type alias Type =
    { name : Maybe String, kind : String, ofType : Maybe OfType }


type alias OfType =
    { name : Maybe String, kind : String }


introspectionGraphQlQuery : Value
introspectionGraphQlQuery =
    Encode.string "query { __schema { types { name fields { name type { name kind ofType { name kind }} }}}}"


introspectionDataDecoder : D.Decoder (List DataModel)
introspectionDataDecoder =
    D.field "data" (D.field "__schema" (D.field "types" (D.list dataModelDecoder)))


dataModelDecoder : D.Decoder DataModel
dataModelDecoder =
    D.succeed DataModel
        |> required "name" D.string
        |> required "fields" (D.maybe (D.list fieldDecoder))


fieldDecoder : D.Decoder Field
fieldDecoder =
    D.succeed Field
        |> required "name" D.string
        |> required "type" typeDecoder


typeDecoder : D.Decoder Type
typeDecoder =
    D.succeed Type
        |> required "name" (D.maybe D.string)
        |> required "kind" D.string
        |> required "ofType" (D.maybe ofTypeDecoder)


ofTypeDecoder : D.Decoder OfType
ofTypeDecoder =
    D.succeed OfType
        |> required "name" (D.maybe D.string)
        |> required "kind" D.string


type alias Model =
    { count : Int
    , introspectionData : List DataModel
    , filteredIntrospectionData : List ModelTable
    , typeOpened : Maybe ModelTable
    , error : Maybe String
    , searchInput : String
    }


type alias Flags =
    {}


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { count = 0
      , introspectionData = []
      , filteredIntrospectionData = []
      , typeOpened = Nothing
      , error = Nothing
      , searchInput = ""
      }
    , makeRequest
    )


type Msg
    = Increment
    | Decrement
    | UrlRequested UrlRequest
    | UrlChanged Url.Url
    | GotIntrospectionResponse (Result Error (List DataModel))
    | GotDataResponse ModelTable (Result Error ( Int, List (List ( String, Maybe String )) ))
    | OpenType ModelTable
    | PreviousPage
    | NextPage
    | SearchInputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        UrlRequested _ ->
            Debug.todo "branch 'UrlRequested _' not implemented"

        UrlChanged _ ->
            Debug.todo "branch 'UrlChanged _' not implemented"

        GotIntrospectionResponse response ->
            case response of
                Ok introspectionData ->
                    let
                        newDataModels =
                            introspectionData
                                |> List.filter (\data -> String.startsWith "Paginated" data.name)
                                |> List.map (\data -> String.dropLeft 9 data.name)
                    in
                    ( { model
                        | introspectionData = introspectionData
                        , filteredIntrospectionData = getDataModelsWithFields introspectionData newDataModels
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | error = Just (toString err) }, Cmd.none )

        GotDataResponse modelTable (Ok data) ->
            let
                newModelTable =
                    { modelTable
                        | data = data
                    }
            in
            ( { model
                | filteredIntrospectionData = updateModelTable model.filteredIntrospectionData modelTable newModelTable
                , typeOpened = Just newModelTable
              }
            , Cmd.none
            )

        GotDataResponse _ (Err err) ->
            ( { model | error = Just (toString err) }, Cmd.none )

        OpenType modelTable ->
            ( { model | typeOpened = Just modelTable }
            , fetchData Nothing modelTable
            )

        PreviousPage ->
            case model.typeOpened of
                Just modelTable ->
                    let
                        newModelTable =
                            { modelTable
                                | page = modelTable.page - 1
                            }
                    in
                    ( { model
                        | typeOpened =
                            Just newModelTable
                      }
                    , fetchData (Just model.searchInput) newModelTable
                    )

                Nothing ->
                    ( model, Cmd.none )

        NextPage ->
            case model.typeOpened of
                Just modelTable ->
                    let
                        newModelTable =
                            { modelTable
                                | page = modelTable.page + 1
                            }
                    in
                    ( { model
                        | typeOpened =
                            Just newModelTable
                      }
                    , fetchData (Just model.searchInput) newModelTable
                    )

                Nothing ->
                    ( model, Cmd.none )

        SearchInputChanged newSearchInput ->
            ( { model
                | searchInput = newSearchInput
              }
            , case model.typeOpened of
                Just modelTable ->
                    fetchData (Just newSearchInput) modelTable

                Nothing ->
                    Cmd.none
            )


updateModelTable : List ModelTable -> ModelTable -> ModelTable -> List ModelTable
updateModelTable modelTables modelTable newModelTable =
    List.map
        (\modelTable_ ->
            if modelTable_.dataModel.name == modelTable.dataModel.name then
                newModelTable

            else
                modelTable_
        )
        modelTables



-- modelTable.dataModel.fields.maybe.map { |e| "#{data[e]} == filter" }.join(" && ")


{-| Fetch data from the server
-}
graphQlDataQuery : Maybe String -> ModelTable -> Value
graphQlDataQuery maybeFilter modelTable =
    Encode.string
        ("""query { """
            ++ "paginated"
            ++ pluralize modelTable.dataModel.name
            ++ "("
            ++ (case maybeFilter of
                    Nothing ->
                        ""

                    Just "" ->
                        ""

                    Just filter ->
                        "filter: "
                            ++ filtersToGraphQlString filter modelTable.dataModel.fields
                            ++ ", "
               )
            ++ "page: "
            ++ String.fromInt modelTable.page
            ++ """, perPage: """
            ++ String.fromInt modelTable.perPage
            ++ """) { totalCount data { """
            ++ String.join " " (List.map .name <| Maybe.withDefault [] modelTable.dataModel.fields)
            ++ """ } } }"""
        )


filtersToGraphQlString : String -> Maybe (List Field) -> String
filtersToGraphQlString filter maybeFields =
    case maybeFields of
        Just fields ->
            [ "\""
            , fields
                |> List.map (\field -> field.name ++ " % \\\"" ++ filter ++ "\\\"")
                |> String.join " || "
            , "\""
            ]
                |> String.concat

        Nothing ->
            ""


pluralize : String -> String
pluralize str =
    if String.endsWith "y" str then
        String.dropRight 1 str ++ "ies"

    else
        str ++ "s"


fetchData : Maybe String -> ModelTable -> Cmd Msg
fetchData maybeFilter modelTable =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , url = "http://localhost:3000/graphql"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "query", graphQlDataQuery maybeFilter modelTable )
                    , ( "variables", graphQlVariables )
                    ]
                )
        , expect = Http.expectJson (GotDataResponse modelTable) (dataDecoder modelTable.dataModel)
        , timeout = Nothing
        , tracker = Nothing
        }



-- {"data":{"paginatedCurrencies":{"totalCount":1002,"data":[{"createdAt":"2022-11-03 12:10:40 UTC","id":"d6bfc2b8-bcdb-4096-b29a-871a2a0c5675","isoCode":"USD","name":"USD","updatedAt":"2022-11-03 12:10:40 UTC"},{"createdAt":"2022-11-03 12:10:40 UTC","id":"4c4981b0-79bb-4130-8c21-676a04869f34","isoCode":"EUR","name":"EUR","updatedAt":"2022-11-03 12:10:40 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"d0e9ae53-360f-4124-acdf-14fccc4db0d7","isoCode":"txvexdtsakgwvkredhbhzdohetivcqrxfnzyaypplxcxwyuvtw","name":"jjddccaqaoijikobgywcyflvmkcjhlyzvrxksoenfdzgacfchh","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"941a55f3-ec1e-482f-9fb5-6e53e55f899a","isoCode":"oyyduqvpnreyrummkwvrykvuiwmfblnapmytivmioowpvwtgto","name":"koiifjprlqugrypnevxrdmruelaortjfurmjovuqymuuyvgapm","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"f1fc2d45-1aab-457c-8f54-b6ce89013a6b","isoCode":"taeknlmksqmwvrssvfqannqmmwunncfglkouiuncdifpvtyhlc","name":"lndhotzbapwwptgujkvfpsgqyyaezmwtopofoqnranohulkmxv","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"c9089015-494c-4fc6-81c0-3b5f57f17f38","isoCode":"fsjpeurimxsmvhakldkaewoqcdmowawhhnuoltdkqkofmbigkt","name":"fjdgoflnfwwdhvgzaxtrnvwijawfhzmpauodnstezjllvdnqyd","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"84ec0180-86c3-4844-a1a6-5bc4970b36e4","isoCode":"dbkctxhwyaroxwoelzsoqocdgnepcylfzyslxgtzecfojoqpdg","name":"purcvxyjzfycmgmsintfmbvdmatnensmdrmusneuwladjaxnzg","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"b7e5c79d-0f29-4d5e-a9f0-4cd9a251bb4a","isoCode":"tmkwfirlwvqyletbibywofofhulzvdzndtuceftjneuufucfey","name":"velimpjjnmxytohcbxvbtaofnbcedmccieuwkgmmfszkucbhwl","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"f267ab78-a0bf-4b4f-9257-e4a402bb6766","isoCode":"pyxkwyisjitaifpzzyiuiqbdxztvskgrqovwehcowxoxxjmoka","name":"ludhzvvtlbirjncryyzprjmgtkorosceqecimykrnnqpkyvhls","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"8d2036f9-70e0-4cc4-824a-791a51da1820","isoCode":"hheubuwjkiurjqvwqjfpqzypcpjqzvcswpcnyfvsoewrpfjzfv","name":"akksztownfkqcbczshabsddhgwlxbxntjumdesgavfzmrgjibp","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"a84bb48f-f690-45a5-8b8d-3cf59c6db9ed","isoCode":"aohazmwiqbhcawuylfllfbptmeivueagugwpewxmfiliperehr","name":"cfdloakismbvpljmwaennigmiyuckwfafbcljukkucdtqxpaoq","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"d02f1631-7a42-4924-a65a-f5f0ecf014fb","isoCode":"ipfjvecmxmwilfalpuyxjfeyilksrikinwpwrpbpbslxzxdlwt","name":"sxlischmickzrnupwfewmhagscuyzhygcdeblhdrtaukvorwam","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"6914fd74-fc17-45ac-8b97-c12e1a06ea0b","isoCode":"yawtztpvccqcnzartxppxnvmubfbngryavpyyycctuyvujmswx","name":"exkznpvpypmwwtlyssltcwzxniubpnzjnnrvlygqavdjvghsbr","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"60b8d9b2-1cc7-4570-a110-3ebe7a4d0a15","isoCode":"fruotrvrcbrmqahanjnbqqqsmzlqcduvphhmwruljnwfardxqu","name":"tytaihzeailpcpvupljapershkhrqjdftssbeatgketpvgzvyt","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"6d7fb93d-6cc9-4b73-a10b-abeebf666db9","isoCode":"usonpmprjpneeofvlwzxklosikihnhrwwjyddwowzdfndhtliy","name":"nzcsgisedpaekbmtuwufmwaxizavcdttdpnacttsbeldfnjcpl","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"431f9e74-f0c3-4831-b6d7-f88f347e70c3","isoCode":"ehiapajtvfiwfnecenbsbzdonfcjmouynuvlfaqhhvuztadloh","name":"olsygsfvxpnlctfglnmlvwwtusscactadbeqmasroyypvtgpyz","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"c790d743-2fb8-4471-8cea-374a14bd93fb","isoCode":"heobkihscokuawpxapjfmyzvaonvlnwxtdfcifpurmpfhhiuie","name":"ngcjhztmoxbctxidbesbxvxvilhyudsuiuqlzdxxfrkocgrwii","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"5ecd090a-fe6b-4bfd-a3f4-b3ddfd4a7adb","isoCode":"zfglpgneiurrgvhfazozvnsuipysmysvcfffvhoerxzmwovush","name":"aebfkkahcvrmlskxyxlpvmlxilvgpvjjndyadkkzwfjbbhetgj","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"a799aad7-1155-4fb8-b887-e1f03f31b578","isoCode":"nhsyioshglbeyospsklqtannjkxdhpxnptgoydeaqlmhevqavd","name":"ixurninjosuegbqmthbvttqkwrgapbmzxleeeydthlzlkmvbvn","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"15d105df-cacc-4b17-bd38-c7f85f9a507b","isoCode":"sfqjxfgzmdflbyzwdotvonyufblcnadvdmnmhejazqqscogdwd","name":"cjashmeoktfinnnwneoouszxhqjjhbehzzcrrtcefacurvgxiq","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"bae39d4c-911c-43dd-8be1-5a1c81a82fde","isoCode":"yzndqbqrkexkquqiwrtusmqltvqlfwritjkrxfiwhwzbuhxlca","name":"xckagyrlmocyrvzuvsfseawydtqmtvrgegalcgiyxrtcsguszk","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"f71a6445-3c83-4ab0-bcd4-1fc4be35cef9","isoCode":"opcejizycbaqoqmosuxraesajujugzgtugfucswldeijlfgtnv","name":"raboosfxvgpnesldngumylsasuwybvjoqygznocehemuboyxvu","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"89d3db2d-12df-4c62-9186-63785db4d740","isoCode":"yfjxbtbjbpmmsjivmkrlyggktsrvxjwgbovgjjvvkrhgzxuzpj","name":"hmhtrwhuqxvdzvgdsmmgsunmgcpsmbeadnieoztbowzupggljn","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"c5915dcf-7afa-42ac-b5b8-fbd21639fccf","isoCode":"ikveymzphtasfciyedgppstaqxhouqdohkgnvzivysxvvuytdx","name":"tpprjgtmvmpvjvyotnwqoxfrveudwkbuoaepqzovzgriyfoiax","updatedAt":"2022-11-08 08:52:04 UTC"},{"createdAt":"2022-11-08 08:52:04 UTC","id":"865b9e7d-41ad-4fcf-92ec-c7191c20c858","isoCode":"shtlsvheujbonvqdttfrvhbamqquutfxjfxycjdykktmmlvseq","name":"myaofmysoyqyzjxkmjjjjirzthkdrvcweyquwqdnsfzmiqfwpt","updatedAt":"2022-11-08 08:52:04 UTC"}]}}}
-- We need to use D.map2


dataDecoder : DataModel -> D.Decoder ( Int, List (List ( String, Maybe String )) )
dataDecoder dataModel =
    D.field "data"
        (D.field ("paginated" ++ pluralize dataModel.name)
            (D.map2
                (\totalCount data -> ( totalCount, data ))
                (D.field "totalCount" D.int)
                (D.field "data" (D.list (D.keyValuePairs (D.maybe D.string))))
            )
        )


getDataModelsWithFields : List DataModel -> List String -> List ModelTable
getDataModelsWithFields introspectionData dataModels =
    introspectionData
        |> List.filter (\data -> List.member data.name dataModels)
        |> List.map (\data -> { data | fields = data.fields |> Maybe.map (List.filter (\field -> field.type_.kind == "SCALAR" || Maybe.map .kind field.type_.ofType == Just "SCALAR")) })
        |> List.map emptyModelTable


toString : Error -> String
toString err =
    case err of
        Http.BadUrl url ->
            "BadUrl " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus response ->
            "BadStatus " ++ String.fromInt response

        Http.BadBody response ->
            "BadBody " ++ response


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "GRAPHQL ADMIN"
    , body =
        [ Element.layout [ width fill, height fill ] <| graphQLTable model
        ]
    }


displayDataModel : Maybe ModelTable -> Element Msg
displayDataModel maybeModelTable =
    case maybeModelTable of
        Just modelTable ->
            Element.column
                [ width fill
                , height fill
                , Element.padding 10
                ]
                [ displayTable modelTable
                ]

        Nothing ->
            none


displayTable : ModelTable -> Element Msg
displayTable modelTable =
    case modelTable.dataModel.fields of
        Just fields ->
            Element.table [ width fill, height fill ]
                { data = Tuple.second modelTable.data
                , columns =
                    List.map
                        (\field ->
                            { header = el [ Border.solid, Border.width 1, Font.center ] <| text field.name
                            , width = fill
                            , view = displayData field
                            }
                        )
                        fields
                }

        Nothing ->
            none


displayData : Field -> List ( String, Maybe String ) -> Element Msg
displayData field data =
    case List.filter (\( key, _ ) -> key == field.name) data of
        ( _, Just value ) :: _ ->
            el [ Border.solid, Border.width 1, Font.center ] <| text value

        _ ->
            el [ Border.solid, Border.width 1, Font.center ] <| text "null"


graphQLTable : Model -> Element Msg
graphQLTable model =
    Element.column
        [ width fill
        , spacing 10
        ]
        [ displayError model.error
        , displaySearchBar model.searchInput
        , Element.row
            [ width fill
            , spacing 10
            ]
            (List.map displayFieldName model.filteredIntrospectionData)
        , displayDataModel model.typeOpened
        , Maybe.withDefault none <| Maybe.map displayPagination model.typeOpened
        ]


displaySearchBar : String -> Element Msg
displaySearchBar searchInput =
    Element.row
        [ width fill
        , spacing 10
        ]
        [ Input.text
            [ width fill
            ]
            { onChange = SearchInputChanged
            , text = searchInput
            , placeholder = Just <| Input.placeholder [] <| text "Search"
            , label = Input.labelHidden "Search"
            }
        ]


displayPagination : ModelTable -> Element Msg
displayPagination { page, perPage, data } =
    Element.row
        [ width fill
        , spacing 10
        , alignTop
        ]
        [ Element.text <| String.fromInt page
        , Element.text <| String.fromInt perPage
        , Element.text <| String.fromInt <| (Tuple.first data // perPage) + 1

        --     button :
        -- List (Attribute msg)
        -- ->
        --     { onPress : Maybe msg
        --     , label : Element msg
        --     }
        -- -> Element msg
        , Input.button []
            { onPress =
                if page > 1 then
                    Just PreviousPage

                else
                    Nothing
            , label = text "Previous"
            }
        , Input.button []
            { onPress =
                if page < (Tuple.first data // perPage) + 1 then
                    Just NextPage

                else
                    Nothing
            , label = text "Next"
            }
        ]


displayFieldName : ModelTable -> Element Msg
displayFieldName modelTable =
    Element.column
        [ width fill
        , height fill
        , spacing 10
        , pointer
        , onClick (OpenType modelTable)
        ]
        [ Element.text modelTable.dataModel.name
        ]


type alias ModelTable =
    { page : Int
    , perPage : Int
    , dataModel : DataModel
    , data : ( Int, List (List ( String, Maybe String )) )
    }


emptyModelTable : DataModel -> ModelTable
emptyModelTable dataModel =
    { page = 1, perPage = 25, dataModel = dataModel, data = ( 0, [] ) }


displayError : Maybe String -> Element Msg
displayError error =
    case error of
        Just err ->
            el [ alignTop ] <| text err

        Nothing ->
            Element.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
