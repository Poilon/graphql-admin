module Main exposing (main, view)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
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



-- graphQlQuery : Value
-- graphQlQuery =
--     Encode.string "query { __schema { types { name } } }"
-- dataDecoder : D.Decoder (List String)
-- dataDecoder =
--     D.field "data" (D.field "__schema" (D.field "types" (D.list (D.field "name" D.string))))
-- example : {
--   "name": "CreateCustomerPayload",
--   "fields": [
--     {
--       "name": "customer",
--       "type": {
--         "name": "Customer",
--         "kind": "OBJECT"
--       }
--     },
--     {
--       "name": "errors",
--       "type": {
--         "name": null,
--         "kind": "LIST"
--       }
--     }
--   ]
-- },


type alias DataModel =
    { name : String, fields : Maybe (List Field) }


type alias Field =
    { name : String, type_ : Type }


type alias Type =
    { name : Maybe String, kind : String }


introspectionGraphQlQuery : Value
introspectionGraphQlQuery =
    Encode.string "query { __schema { types { name fields { name type { name kind } }}}}"


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


type alias Model =
    { count : Int
    , introspectionData : List DataModel
    , filteredIntrospectionData : List ModelTable
    , typeOpened : Maybe ModelTable
    , error : Maybe String
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
      }
    , makeRequest
    )


type Msg
    = Increment
    | Decrement
    | UrlRequested UrlRequest
    | UrlChanged Url.Url
    | GotIntrospectionResponse (Result Error (List DataModel))
    | GotDataResponse ModelTable (Result Error (List (List ( String, Maybe String ))))
    | OpenType ModelTable


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
            , fetchData modelTable
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


graphQlDataQuery : ModelTable -> Value
graphQlDataQuery modelTable =
    Encode.string
        ("""query { """
            ++ "paginated"
            ++ pluralize modelTable.dataModel.name
            ++ """(page: """
            ++ String.fromInt modelTable.page
            ++ """, perPage: """
            ++ String.fromInt modelTable.perPage
            ++ """) { data { """
            ++ String.join " " (List.map .name <| Maybe.withDefault [] modelTable.dataModel.fields)
            ++ """ } } }"""
        )


pluralize : String -> String
pluralize str =
    if String.endsWith "y" str then
        String.dropRight 1 str ++ "ies"

    else
        str ++ "s"


fetchData : ModelTable -> Cmd Msg
fetchData modelTable =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , url = "http://localhost:3000/graphql"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "query", graphQlDataQuery modelTable )
                    , ( "variables", graphQlVariables )
                    ]
                )
        , expect = Http.expectJson (GotDataResponse modelTable) (dataDecoder modelTable.dataModel)
        , timeout = Nothing
        , tracker = Nothing
        }


dataDecoder : DataModel -> D.Decoder (List (List ( String, Maybe String )))
dataDecoder dataModel =
    D.field "data"
        (D.field ("paginated" ++ pluralize dataModel.name)
            (D.field "data" (D.list (D.keyValuePairs (D.maybe D.string))))
        )



-- type alias GraphQLData =
--     { key : String, value : Maybe String }


getDataModelsWithFields : List DataModel -> List String -> List ModelTable
getDataModelsWithFields introspectionData dataModels =
    introspectionData
        |> List.filter (\data -> List.member data.name dataModels)
        |> List.map (\data -> { data | fields = data.fields |> Maybe.map (List.filter (\field -> field.type_.kind == "SCALAR")) })
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
                { data = modelTable.data
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
        , height fill
        , spacing 10
        ]
        [ displayError model.error
        , Element.row
            [ width fill
            , spacing 10
            ]
            (List.map displayFieldName model.filteredIntrospectionData)
        , displayDataModel model.typeOpened
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
    , data : List (List ( String, Maybe String ))
    }


emptyModelTable : DataModel -> ModelTable
emptyModelTable dataModel =
    { page = 1, perPage = 25, dataModel = dataModel, data = [] }


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
