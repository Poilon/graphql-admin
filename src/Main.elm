module Main exposing (main, view)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, option, select)
import Html.Attributes
import Html.Events
import Http exposing (Error)
import Json.Decode as D
import Json.Decode.Pipeline as D exposing (required, requiredAt)
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


introspectionGraphQlQuery : Value
introspectionGraphQlQuery =
    Encode.string """query {
    __schema {
        types {
            name
            fields {
                name
                type {
                    name
                    kind
                    ofType {
                        name
                        kind
                    }
                }
            }
        }
        mutationType {
            name
            fields {
                name
                type {
                    name
                    kind
                    ofType {
                        name
                        kind
                    }
                }
            }
        }
    }
}"""


type alias IntrospectionData =
    { types : List DataModel, mutationType : List DataModel }


type alias DataModel =
    { name : String, fields : List Field }


type alias Field =
    { name : String, type_ : Type }


type alias Type =
    { name : Maybe String, kind : String, ofType : Maybe OfType }


type alias OfType =
    { name : Maybe String, kind : String }


type alias Data =
    { totalCount : Int
    , dataList : List DataRow
    }


type alias DataRow =
    List { key : String, value : Maybe String }


introspectionDataDecoder : D.Decoder IntrospectionData
introspectionDataDecoder =
    D.succeed IntrospectionData
        |> requiredAt [ "data", "__schema", "types" ] (D.list dataModelDecoder)
        |> requiredAt [ "data", "__schema", "mutationType" ] (D.list dataModelDecoder)


dataModelDecoder : D.Decoder DataModel
dataModelDecoder =
    D.succeed DataModel
        |> required "name" D.string
        |> D.optional "fields" (D.list fieldDecoder) []


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
    , introspectionData : IntrospectionData
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
      , introspectionData = IntrospectionData [] []
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
    | GotIntrospectionResponse (Result Error IntrospectionData)
    | GotDataResponse ModelTable (Result Error Data)
    | OpenType ModelTable
    | PreviousPage
    | NextPage
    | SearchInputChanged String
    | UpdatePerPage String
    | OrderBy String


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
                            introspectionData.types
                                |> List.filter (\data -> String.startsWith "Paginated" data.name)
                                |> List.map (\data -> String.dropLeft 9 data.name)
                    in
                    ( { model
                        | introspectionData = introspectionData
                        , filteredIntrospectionData = getDataModelsWithFields introspectionData.types newDataModels
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

        UpdatePerPage newPerPage ->
            case model.typeOpened of
                Just modelTable ->
                    let
                        newModelTable =
                            { modelTable
                                | perPage = Maybe.withDefault modelTable.perPage <| String.toInt newPerPage
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

        OrderBy newOrderBy ->
            case model.typeOpened of
                Just modelTable ->
                    let
                        oldOrderDirection =
                            modelTable.orderDirection

                        newOrderDirection =
                            if modelTable.orderBy == newOrderBy then
                                if oldOrderDirection == Asc then
                                    Desc

                                else
                                    Asc

                            else
                                Asc

                        newModelTable =
                            { modelTable
                                | orderBy = newOrderBy
                                , orderDirection = newOrderDirection
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
            ++ ", perPage: "
            ++ String.fromInt modelTable.perPage
            ++ ", orderBy: \""
            ++ modelTable.orderBy
            ++ " "
            ++ orderDirectionToString modelTable.orderDirection
            ++ "\""
            ++ ") { totalCount data { "
            ++ String.join " " (List.map .name modelTable.dataModel.fields)
            ++ " } } }"
        )


filtersToGraphQlString : String -> List Field -> String
filtersToGraphQlString filter fields =
    case fields of
        [] ->
            ""

        fields_ ->
            [ "\""
            , fields_
                |> List.map (\field -> field.name ++ " % \\\"" ++ filter ++ "\\\"")
                |> String.join " || "
            , "\""
            ]
                |> String.concat


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


dataRowDecoder : D.Decoder DataRow
dataRowDecoder =
    D.keyValuePairs (D.maybe D.string) |> D.map (List.map (\( key, value ) -> { key = key, value = value }))


dataDecoder : DataModel -> D.Decoder Data
dataDecoder dataModel =
    D.succeed Data
        |> requiredAt [ "data", "paginated" ++ pluralize dataModel.name, "totalCount" ] D.int
        |> requiredAt [ "data", "paginated" ++ pluralize dataModel.name, "data" ] (D.list dataRowDecoder)


getDataModelsWithFields : List DataModel -> List String -> List ModelTable
getDataModelsWithFields introspectionData dataModels =
    introspectionData
        |> List.filter (\data -> List.member data.name dataModels)
        |> List.map (\data -> { data | fields = data.fields |> List.filter (\field -> field.type_.kind == "SCALAR" || Maybe.map .kind field.type_.ofType == Just "SCALAR") })
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


view : Model -> Document Msg
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
    Element.table [ width fill, height fill ]
        { data = modelTable.data.dataList
        , columns =
            List.map
                (\field ->
                    { header =
                        row
                            [ Border.solid
                            , Border.width 1
                            , Font.center
                            , onClick <| OrderBy field.name
                            , pointer
                            ]
                        <|
                            [ text field.name
                            , maybeShowCaret modelTable field.name
                            ]
                    , width = fill
                    , view = displayData field
                    }
                )
                modelTable.dataModel.fields
        }


maybeShowCaret : ModelTable -> String -> Element Msg
maybeShowCaret modelTable fieldName =
    if modelTable.orderBy == fieldName then
        if modelTable.orderDirection == Asc then
            Element.el [ Font.size 10, alignRight ] <| text "▲"

        else
            Element.el [ Font.size 10, alignRight ] <| text "▼"

    else
        none


displayData : Field -> DataRow -> Element Msg
displayData field data =
    el [ Border.solid, Border.width 1, Font.center ] <|
        text <|
            case data |> List.filter (\d -> d.key == field.name) |> List.head of
                Just d ->
                    case d.value of
                        Just value ->
                            value

                        Nothing ->
                            "null"

                Nothing ->
                    "null"


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


intToOption : Int -> Int -> Html Msg
intToOption currentPerPage v =
    option [ Html.Attributes.value (String.fromInt v), Html.Attributes.selected (currentPerPage == v) ] [ Html.text (String.fromInt v) ]


displayPagination : ModelTable -> Element Msg
displayPagination ({ page, perPage, data } as modelTable) =
    Element.column []
        [ Element.row
            [ width fill
            , spacing 10
            , alignTop
            ]
            [ Element.text <| "Page: " ++ String.fromInt page
            , Element.text <| "Par Page: "

            -- Select per page
            , html <|
                select
                    [ Html.Events.onInput UpdatePerPage ]
                    ([ 25, 50, 100 ] |> List.map (intToOption perPage))
            , Element.text <| " Total pages: " ++ (String.fromInt <| (data.totalCount // perPage) + 1)

            --     button :
            -- List (Attribute msg)
            -- ->
            --     { onPress : Maybe msg
            --     , label : Element msg
            --     }
            -- -> Element msg
            ]
        , Element.row
            [ width fill, spacing 10 ]
            [ Input.button []
                { onPress =
                    if page > 1 then
                        Just PreviousPage

                    else
                        Nothing
                , label = text "Previous"
                }
            , Input.button []
                { onPress =
                    if page < (data.totalCount // perPage) + 1 then
                        Just NextPage

                    else
                        Nothing
                , label = text "Next"
                }
            ]
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
    , orderBy : String
    , orderDirection : OrderDirection
    , dataModel : DataModel
    , data : Data
    }


type OrderDirection
    = Asc
    | Desc


orderDirectionToString : OrderDirection -> String
orderDirectionToString orderDirection =
    case orderDirection of
        Asc ->
            "asc"

        Desc ->
            "desc"


emptyModelTable : DataModel -> ModelTable
emptyModelTable dataModel =
    { page = 1
    , perPage = 25
    , orderBy = "createdAt"
    , orderDirection = Desc
    , dataModel = dataModel
    , data = Data 0 []
    }


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



-- {
--   "data": {
--     "__schema": {
--       "mutationType": {
--         "name": "Mutation",
--         "fields": [
--           {
--             "name": "createCustomer",
--             "args": [
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "CustomerInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "createFolder",
--             "args": [
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "FolderInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "createMission",
--             "args": [
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "MissionInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "createTask",
--             "args": [
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "TaskInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "createUser",
--             "args": [
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "UserInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "createWorkspace",
--             "args": [
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "WorkspaceInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "createWorkspaceUser",
--             "args": [
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "WorkspaceUserInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "destroyCustomer",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               }
--             ]
--           },
--           {
--             "name": "destroyFolder",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               }
--             ]
--           },
--           {
--             "name": "destroyMission",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               }
--             ]
--           },
--           {
--             "name": "destroyTask",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               }
--             ]
--           },
--           {
--             "name": "destroyUser",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               }
--             ]
--           },
--           {
--             "name": "destroyWorkspace",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               }
--             ]
--           },
--           {
--             "name": "destroyWorkspaceUser",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               }
--             ]
--           },
--           {
--             "name": "updateCustomer",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               },
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "CustomerInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "updateFolder",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               },
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "FolderInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "updateMission",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               },
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "MissionInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "updateTask",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               },
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "TaskInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "updateUser",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               },
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "UserInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "updateWorkspace",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               },
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "WorkspaceInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           },
--           {
--             "name": "updateWorkspaceUser",
--             "args": [
--               {
--                 "name": "id",
--                 "type": {
--                   "name": null,
--                   "kind": "NON_NULL",
--                   "ofType": {
--                     "name": "String",
--                     "kind": "SCALAR"
--                   }
--                 }
--               },
--               {
--                 "name": "attributes",
--                 "type": {
--                   "name": "WorkspaceUserInputType",
--                   "kind": "INPUT_OBJECT",
--                   "ofType": null
--                 }
--               }
--             ]
--           }
--         ]
--       }
--     }
--   }
-- }
