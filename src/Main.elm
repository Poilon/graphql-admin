module Main exposing (main, view)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Color exposing (..)
import Common.Events as Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import FontAwesome as FA
import FontAwesome.Solid as FA
import Html exposing (Html, option, select)
import Html.Attributes
import Html.Events
import Http exposing (Error)
import Json.Decode as D
import Json.Decode.Pipeline as D exposing (required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Palette.Utils exposing (displayIcon)
import String.Case as String exposing (toCamelCaseLower)
import Url


graphQlVariables : Value
graphQlVariables =
    Encode.object [ ( "loggedUserId", Encode.string "854b1efb-faaf-47ca-9abf-b1563eec71d3" ) ]


makeRequest : Cmd Msg
makeRequest =
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
            inputFields {
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
            enumValues {
                name
            }
        }
        mutationType {
            name
            fields {
                name
                args {
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
    }
}"""


type alias IntrospectionData =
    { dataModels : List DataModel, mutationType : List Mutation }


type alias DataModel =
    { name : String, fields : List Field, inputFields : List InputField, enumValues : List EnumValue }


type alias EnumValue =
    { name : String }


type alias Field =
    { name : String, type_ : Type }


type alias InputField =
    { name : String, type_ : Type }


type alias Type =
    { name : Maybe String, kind : String, ofType : Maybe OfType }


type alias OfType =
    { name : Maybe String, kind : String }


type alias Mutation =
    { name : String, args : List MutationArgument }


type alias MutationArgument =
    { name : String, type_ : Type }


type alias Data =
    { totalCount : Int
    , dataList : List DataRow
    }


type alias DataRow =
    { row : List { key : String, value : Maybe String } }


type alias DataModelWithMutations =
    { name : String, fields : List Field, inputFields : List InputField, mutations : List Mutation }


fillModelTablesWithMutations : List ModelTable -> List Mutation -> List ModelTable
fillModelTablesWithMutations modelTables mutations =
    List.map
        (\modelTable ->
            let
                dataModelWithMutations =
                    modelTable.dataModelWithMutations
            in
            { modelTable
                | dataModelWithMutations =
                    { dataModelWithMutations
                        | mutations =
                            List.filter (\mutation -> mutation.name |> String.endsWith modelTable.dataModelWithMutations.name) mutations
                    }
            }
        )
        modelTables


introspectionDataDecoder : D.Decoder IntrospectionData
introspectionDataDecoder =
    D.succeed IntrospectionData
        |> requiredAt [ "data", "__schema", "types" ] (D.list dataModelDecoder)
        |> requiredAt [ "data", "__schema", "mutationType", "fields" ] (D.list mutationDecoder)


dataModelDecoder : D.Decoder DataModel
dataModelDecoder =
    D.succeed DataModel
        |> required "name" D.string
        |> D.optional "fields" (D.list fieldDecoder) []
        |> D.optional "inputFields" (D.list fieldDecoder) []
        |> D.optional "enumValues" (D.list enumValueDecoder) []


enumValueDecoder : D.Decoder EnumValue
enumValueDecoder =
    D.succeed EnumValue
        |> required "name" D.string


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


mutationDecoder : D.Decoder Mutation
mutationDecoder =
    D.succeed Mutation
        |> required "name" D.string
        |> required "args" (D.list mutationArgumentDecoder)


mutationArgumentDecoder : D.Decoder MutationArgument
mutationArgumentDecoder =
    D.succeed MutationArgument
        |> required "name" D.string
        |> required "type" typeDecoder


type alias Model =
    { count : Int
    , introspectionData : IntrospectionData
    , filteredIntrospectionData : List ModelTable
    , typeOpened : Maybe ModelTable
    , error : Maybe String
    , searchInput : String
    , modal : Maybe Modal
    , enumTypes : List EnumType
    }


type alias EnumType =
    { name : String
    , values : List String
    }


type Modal
    = CreateModal Mutation DataModelWithMutations (List Input)


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
      , modal = Nothing
      , enumTypes = []
      }
    , makeRequest
    )


type Msg
    = NoOp
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
    | DataCellChanged String String String
    | Destroy Mutation Id DataModelWithMutations
    | GotMutationResponse Id (Result Error DataRow)
    | UpdateMutation Mutation Id DataRow DataModelWithMutations
    | GotUpdateResponse Id (Result Error DataRow)
    | OpenModal Mutation DataModelWithMutations (List Input)
    | CloseModal
    | UpdateInputField String String
    | CreateMutation Mutation DataModelWithMutations (List Input)
    | GotCreateResponse (Result Error DataRow)


type Id
    = Id String


idToString : Id -> String
idToString (Id id) =
    id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlRequested _ ->
            Debug.todo "branch 'UrlRequested _' not implemented"

        UrlChanged _ ->
            Debug.todo "branch 'UrlChanged _' not implemented"

        GotIntrospectionResponse response ->
            case response of
                Ok introspectionData ->
                    let
                        newDataModels =
                            introspectionData.dataModels
                                |> List.filter (\data -> String.startsWith "Paginated" data.name)
                                |> List.map (\data -> String.dropLeft 9 data.name)

                        dataModelsWithInputsFields =
                            introspectionData.dataModels
                                |> List.filter (\data -> String.endsWith "InputType" data.name)
                                |> List.map (\data -> { data | name = String.dropRight 9 data.name })

                        enumTypes =
                            introspectionData.dataModels
                                |> List.filter (\data -> data.enumValues /= [])
                                |> List.map (\data -> { name = data.name, values = List.map .name data.enumValues })
                    in
                    ( { model
                        | introspectionData = introspectionData
                        , filteredIntrospectionData = fillModelTablesWithMutations (getDataModelsWithFields introspectionData.dataModels dataModelsWithInputsFields (Debug.log "newDataModels" newDataModels)) introspectionData.mutationType
                        , enumTypes = enumTypes
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

        Destroy mutation id dataName ->
            ( model, callDestroyMutation mutation id dataName )

        GotMutationResponse id (Ok _) ->
            ( model
            , case model.typeOpened of
                Just modelTable ->
                    fetchData (Just model.searchInput) modelTable

                Nothing ->
                    Cmd.none
            )

        GotMutationResponse id (Err err) ->
            ( { model | error = Just (toString err) }, Cmd.none )

        UpdateMutation mutation id dataRow dataName ->
            ( model, callUpdateMutation mutation id dataRow dataName )

        GotUpdateResponse id (Ok _) ->
            ( model
            , case model.typeOpened of
                Just modelTable ->
                    fetchData (Just model.searchInput) modelTable

                Nothing ->
                    Cmd.none
            )

        GotUpdateResponse id (Err err) ->
            ( { model | error = Just (toString err) }, Cmd.none )

        DataCellChanged dataModelName fieldName newValue ->
            case model.typeOpened of
                Just modelTable ->
                    let
                        data =
                            modelTable.data

                        dataList =
                            modelTable.data.dataList

                        dataListWithNewValue =
                            List.map
                                (\dataRow ->
                                    let
                                        row =
                                            dataRow.row

                                        rowWithNewValue =
                                            List.map
                                                (\field ->
                                                    if field.key == fieldName then
                                                        { field | value = Just newValue }

                                                    else
                                                        field
                                                )
                                                row
                                    in
                                    { dataRow | row = rowWithNewValue }
                                )
                                dataList

                        newData =
                            { data | dataList = dataListWithNewValue }

                        newModelTable =
                            { modelTable | data = newData }
                    in
                    ( { model
                        | filteredIntrospectionData = updateModelTable model.filteredIntrospectionData modelTable newModelTable
                        , typeOpened = Just newModelTable
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        OpenModal mutation dataModelWithMutations inputs ->
            ( { model
                | modal = Just <| CreateModal mutation dataModelWithMutations inputs
              }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modal = Nothing }, Cmd.none )

        UpdateInputField inputFieldLabel newValue ->
            case model.modal of
                Just (CreateModal mutation dataModelWithMutations inputs) ->
                    let
                        newInputs =
                            List.map
                                (\input ->
                                    if input.label == inputFieldLabel then
                                        { input | value = newValue }

                                    else
                                        input
                                )
                                inputs
                    in
                    ( { model
                        | modal = Just <| CreateModal mutation dataModelWithMutations newInputs
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        CreateMutation mutation dataModelWithMutations inputs ->
            ( model, callCreateMutation mutation dataModelWithMutations inputs )

        GotCreateResponse (Ok _) ->
            ( model
            , case model.typeOpened of
                Just modelTable ->
                    fetchData (Just model.searchInput) modelTable

                Nothing ->
                    Cmd.none
            )

        GotCreateResponse (Err err) ->
            ( { model | error = Just (toString err) }, Cmd.none )


encodeFields : List Field -> String
encodeFields fields =
    fields
        |> List.map .name
        |> String.join " "


callDestroyMutation : Mutation -> Id -> DataModelWithMutations -> Cmd Msg
callDestroyMutation mutation id dataModel =
    case List.head mutation.args of
        Just mutationArgument ->
            if mutationArgument.name == "id" && mutationArgument.type_.kind == "NON_NULL" then
                Http.request
                    { method = "POST"
                    , headers =
                        [ Http.header "Content-Type" "application/json"
                        ]
                    , url = "http://localhost:3000/graphql"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "query"
                                  , Encode.string
                                        ("mutation { "
                                            ++ mutation.name
                                            ++ "(id: \""
                                            ++ idToString id
                                            ++ "\") { "
                                            ++ String.toCamelCaseLower dataModel.name
                                            ++ " { "
                                            ++ encodeFields dataModel.fields
                                            ++ " } } }"
                                        )
                                  )
                                , ( "variables", graphQlVariables )
                                ]
                            )
                    , expect = Http.expectJson (GotMutationResponse id) (dataModelFromMutationDecoder mutation dataModel.name)
                    , timeout = Nothing
                    , tracker = Nothing
                    }

            else
                Debug.todo "mutation argument is not an id"

        Nothing ->
            Debug.todo "mutation has no argument"


callUpdateMutation : Mutation -> Id -> DataRow -> DataModelWithMutations -> Cmd Msg
callUpdateMutation mutation id dataRow dataModel =
    case List.head (Debug.log "mutationsArgs" mutation.args) of
        Just mutationArgument ->
            if mutationArgument.name == "id" && mutationArgument.type_.kind == "NON_NULL" then
                Http.request
                    { method = "POST"
                    , headers = [ Http.header "Content-Type" "application/json" ]
                    , url = "http://localhost:3000/graphql"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "query"
                                  , Encode.string
                                        ("mutation { "
                                            ++ mutation.name
                                            ++ "(id: \""
                                            ++ idToString id
                                            ++ "\", "
                                            ++ "attributes: { "
                                            ++ Debug.log "encodeDataRow dataRow.row dataModel.inputFields" (encodeDataRow dataRow.row dataModel.inputFields)
                                            ++ " }) { "
                                            ++ Debug.log "String.toCamelCaseLower dataModel.name" (String.toCamelCaseLower dataModel.name)
                                            ++ " { "
                                            ++ Debug.log "encodeFields dataModel.inputFields" (encodeFields dataModel.inputFields)
                                            ++ " } } }"
                                        )
                                  )
                                , ( "variables", graphQlVariables )
                                ]
                            )
                    , expect = Http.expectJson (GotUpdateResponse id) (dataModelFromMutationDecoder mutation dataModel.name)
                    , timeout = Nothing
                    , tracker = Nothing
                    }

            else
                Debug.todo "mutation argument is not an id"

        Nothing ->
            Debug.todo "mutation has no argument"


callCreateMutation : Mutation -> DataModelWithMutations -> List Input -> Cmd Msg
callCreateMutation mutation dataModel inputs =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = "http://localhost:3000/graphql"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "query"
                      , Encode.string
                            ("mutation { "
                                ++ mutation.name
                                ++ "(attributes: { "
                                ++ encodeInputs inputs
                                ++ " }) { "
                                ++ String.toCamelCaseLower dataModel.name
                                ++ " { "
                                ++ encodeFields dataModel.fields
                                ++ " } } }"
                            )
                      )
                    , ( "variables", graphQlVariables )
                    ]
                )
        , expect = Http.expectJson GotCreateResponse (dataModelFromMutationDecoder mutation dataModel.name)
        , timeout = Nothing
        , tracker = Nothing
        }


encodeDataRow : List { key : String, value : Maybe String } -> List InputField -> String
encodeDataRow row fields =
    row
        |> List.map (\field -> Maybe.map2 (\value fieldType -> field.key ++ ": " ++ encodeValue value fieldType) field.value (getInputFieldType field.key fields))
        |> List.filterMap identity
        |> String.join ", "


getInputFieldType : String -> List InputField -> Maybe Type
getInputFieldType fieldName fields =
    case List.filter (\field -> field.name == fieldName) fields of
        field :: _ ->
            Just field.type_

        [] ->
            Nothing


encodeValue : String -> { name : Maybe String, kind : String, ofType : Maybe OfType } -> String
encodeValue value { name, kind, ofType } =
    case ( name, kind, ofType ) of
        ( Just "Int", _, _ ) ->
            value

        ( Just "Float", _, _ ) ->
            value

        ( Just "String", _, _ ) ->
            "\"" ++ value ++ "\""

        ( Just "Boolean", _, _ ) ->
            value

        ( Just "ID", _, _ ) ->
            "\"" ++ value ++ "\""

        ( _, "NON_NULL", Just ofType_ ) ->
            encodeValueByOfType value ofType_

        ( _, "LIST", Just ofType_ ) ->
            "[" ++ encodeValueByOfType value ofType_ ++ "]"

        _ ->
            Debug.todo "encodeValue: unknown type"


encodeValueByOfType : String -> { name : Maybe String, kind : String } -> String
encodeValueByOfType value { name, kind } =
    case ( name, kind ) of
        ( Just "Int", _ ) ->
            value

        ( Just "Float", _ ) ->
            value

        ( Just "String", _ ) ->
            "\"" ++ value ++ "\""

        ( Just "Boolean", _ ) ->
            value

        ( Just "ID", _ ) ->
            "\"" ++ value ++ "\""

        _ ->
            Debug.todo "encodeValueByOfType: unknown type"


encodeInputs : List Input -> String
encodeInputs inputs =
    inputs
        |> List.map (\input -> input.label ++ ": " ++ encodeValue input.value input.type_)
        |> String.join ", "


dataModelFromMutationDecoder : Mutation -> String -> D.Decoder DataRow
dataModelFromMutationDecoder mutation dataName =
    D.succeed DataRow
        |> D.requiredAt [ "data", toCamelCaseLower mutation.name, toCamelCaseLower dataName ] instanceDecoder


updateModelTable : List ModelTable -> ModelTable -> ModelTable -> List ModelTable
updateModelTable modelTables modelTable newModelTable =
    List.map
        (\modelTable_ ->
            if modelTable_.dataModelWithMutations.name == modelTable.dataModelWithMutations.name then
                newModelTable

            else
                modelTable_
        )
        modelTables


graphQlDataQuery : Maybe String -> ModelTable -> Value
graphQlDataQuery maybeFilter modelTable =
    Encode.string
        ("""query { """
            ++ "paginated"
            ++ pluralize modelTable.dataModelWithMutations.name
            ++ "("
            ++ (case maybeFilter of
                    Nothing ->
                        ""

                    Just "" ->
                        ""

                    Just filter ->
                        "filter: "
                            ++ filtersToGraphQlString filter modelTable.dataModelWithMutations.fields
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
            ++ String.join " " (List.map .name modelTable.dataModelWithMutations.fields)
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
        , expect = Http.expectJson (GotDataResponse modelTable) (dataDecoder modelTable.dataModelWithMutations)
        , timeout = Nothing
        , tracker = Nothing
        }


instanceDecoder : D.Decoder (List { key : String, value : Maybe String })
instanceDecoder =
    D.keyValuePairs (D.maybe D.string)
        |> D.map (List.map (\( key, value ) -> { key = key, value = value }))


dataRowDecoder : D.Decoder DataRow
dataRowDecoder =
    instanceDecoder |> D.map DataRow


dataDecoder : DataModelWithMutations -> D.Decoder Data
dataDecoder dataModel =
    D.succeed Data
        |> requiredAt [ "data", "paginated" ++ pluralize dataModel.name, "totalCount" ] D.int
        |> requiredAt [ "data", "paginated" ++ pluralize dataModel.name, "data" ] (D.list dataRowDecoder)


getDataModelsWithFields : List DataModel -> List DataModel -> List String -> List ModelTable
getDataModelsWithFields introspectionData datasModelsWithInputFields dataModels =
    introspectionData
        |> List.filter (\data -> List.member data.name dataModels)
        |> List.map
            (\data ->
                { data
                    | fields = data.fields |> List.filter (\field -> field.type_.kind == "SCALAR" || Maybe.map .kind field.type_.ofType == Just "SCALAR")
                    , inputFields = datasModelsWithInputFields |> List.filter (\dataModelWithInputFields -> dataModelWithInputFields.name == data.name) |> List.map .inputFields |> List.concat
                }
            )
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
        [ Element.layout
            [ width fill
            , height fill
            , inFront <| displayModal model.enumTypes model.modal
            ]
          <|
            graphQLTable model
        ]
    }



-- callUpdateMutation : Mutation -> Id -> DataRow -> DataModelWithMutations -> Cmd Msg
-- callUpdateMutation mutation id dataRow dataModel =
--     case List.head (Debug.log "mutationsArgs" mutation.args) of
--         Just mutationArgument ->
--             if mutationArgument.name == "id" && mutationArgument.type_.kind == "NON_NULL" then
--                 Http.request
--                     { method = "POST"
--                     , headers = [ Http.header "Content-Type" "application/json" ]
--                     , url = "http://localhost:3000/graphql"
--                     , body =
--                         Http.jsonBody
--                             (Encode.object
--                                 [ ( "query"
--                                   , Encode.string
--                                         ("mutation { "
--                                             ++ mutation.name
--                                             ++ "(id: \""
--                                             ++ idToString id
--                                             ++ "\", "
--                                             ++ "attributes: { "
--                                             ++ Debug.log "encodeDataRow dataRow.row dataModel.inputFields" (encodeDataRow dataRow.row dataModel.inputFields)
--                                             ++ " }) { "
--                                             ++ Debug.log "String.toCamelCaseLower dataModel.name" (String.toCamelCaseLower dataModel.name)
--                                             ++ " { "
--                                             ++ Debug.log "encodeFields dataModel.inputFields" (encodeFields dataModel.inputFields)
--                                             ++ " } } }"
--                                         )
--                                   )
--                                 , ( "variables", graphQlVariables )
--                                 ]
--                             )
--                     , expect = Http.expectJson (GotUpdateResponse id) (dataModelFromMutationDecoder mutation dataModel.name)
--                     , timeout = Nothing
--                     , tracker = Nothing
--                     }
--             else
--                 Debug.todo "mutation argument is not an id"
--         Nothing ->
--             Debug.todo "mutation has no argument"
-- button :
--     List (Attribute msg)
--     ->
--         { onPress : Maybe msg
--         , label : Element msg
--         }
--     -> Element msg
-- button attrs { onPress, label }


displayModal : List EnumType -> Maybe Modal -> Element Msg
displayModal enumTypes maybeModal =
    case maybeModal of
        Nothing ->
            Element.none

        Just (CreateModal mutation dataModelWithMutations inputs) ->
            column [ width fill, height fill, Background.color <| Element.rgba 0 0 0 0.5, onClick CloseModal ]
                [ column
                    [ Events.greedyOnClick NoOp
                    , width <| px 800
                    , Background.color <| Element.rgb 1 1 1
                    , Border.rounded 8
                    , centerX
                    , centerY
                    ]
                    [ text <| mutation.name
                    , createMutationInputs mutation dataModelWithMutations inputs enumTypes
                    , row [ centerY, centerX ]
                        [ Input.button [] { onPress = Just <| CreateMutation mutation dataModelWithMutations inputs, label = text "Create" }
                        , Input.button [] { onPress = Just CloseModal, label = text "Cancel" }
                        ]
                    ]
                ]



-- type alias DataModelWithMutations =
--     { name : String, fields : List Field, inputFields : List InputField, mutations : List Mutation }


createMutationInputs : Mutation -> DataModelWithMutations -> List Input -> List EnumType -> Element Msg
createMutationInputs mutation dataModelWithMutations inputs enumTypes =
    column [] <| List.map (createMutationInput enumTypes) inputs



-- inputText :
--     List (Attribute msg)
--     ->
--         { onChange : String -> msg
--         , text : String
--         , placeholder : Maybe (Placeholder msg)
--         , label : Label msg
--         }
-- Input.text
--     { onChange = \text -> UpdateInputField mutation dataRow dataModelWithMutations inputField text
--     , text = ""
--     , placeholder = Nothing
--     , label = text inputField.name
--     }
-- intToOption : Int -> Int -> Html Msg
-- intToOption currentPerPage v =
--     option
--         [ Html.Attributes.value (String.fromInt v)
--         , Html.Attributes.selected (currentPerPage == v)
--         ]
-- selectView = html <|
--         select
--             [ Html.Events.onInput UpdatePerPage ]
--             ([ 25, 50, 100 ] |> List.map (intToOption perPage))

-- name = "status"
-- ▾type_ = {
-- kind = "ENUM"
-- name = Just "FolderStatus"
-- ofType = Nothing
-- }
createMutationInput : List EnumType -> Input -> Element Msg
createMutationInput enumTypes { label, value, type_ } =
    -- This works only if the input is a scalar
    -- Input.text []
    --     { onChange = UpdateInputField label
    --     , text = value
    --     , placeholder = Nothing
    --     , label = Input.labelLeft [] <| text label
    --     }
    case type_.kind of
        "SCALAR" ->
            Input.text []
                { onChange = UpdateInputField label
                , text = value
                , placeholder = Nothing
                , label = Input.labelLeft [] <| text label
                }

        "ENUM" ->
            case type_.name of
                Just enumName ->
                    case List.filter (\enumType -> enumType.name == enumName) enumTypes of
                        enumType :: _ ->
                            html <|
                            select
                                [ Html.Events.onInput <| UpdateInputField label ]
                                (List.map (enumToOption value) enumType.values)

                        [] ->
                            text "Enum not found"

                Nothing ->
                    text "Enum name not found"


        _ ->
            Debug.todo "createMutationInput: type_.kind is not a scalar or an enum"


enumToOption : String -> String -> Html Msg
enumToOption currentValue enum =
    option
        [ Html.Attributes.value enum
        , Html.Attributes.selected (currentValue == enum)
        ]
        [ Html.text enum ]


displayDataModel : Maybe ModelTable -> Element Msg
displayDataModel maybeModelTable =
    case maybeModelTable of
        Just modelTable ->
            column
                [ width fill
                , height fill
                , Element.padding 10
                ]
                [ displayTable modelTable
                , displayCreateButton modelTable
                ]

        Nothing ->
            none


displayCreateButton : ModelTable -> Element Msg
displayCreateButton modelTable =
    case
        modelTable.dataModelWithMutations.mutations
            |> List.filter (String.startsWith "create" << .name)
            |> List.head
    of
        Just mutation ->
            Input.button
                []
                { label = text mutation.name
                , onPress = Just <| OpenModal mutation modelTable.dataModelWithMutations modelTable.inputs
                }

        Nothing ->
            none


type FieldOrMutation
    = DataField Field
    | MutationField Mutation


displayTable : ModelTable -> Element Msg
displayTable modelTable =
    let
        fields =
            List.map DataField modelTable.dataModelWithMutations.fields

        mutationsWithoutCreateAndUpdate =
            modelTable.dataModelWithMutations.mutations
                |> List.map MutationField
    in
    Element.table [ width fill, height fill ]
        { data = modelTable.data.dataList
        , columns =
            List.map
                (\fieldOrMutation ->
                    case fieldOrMutation of
                        DataField field ->
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
                            , view = displayData field modelTable.dataModelWithMutations.name
                            }

                        MutationField mutation ->
                            { header =
                                row
                                    [ Border.solid
                                    , Border.width 1
                                    , Font.center
                                    ]
                                <|
                                    [ text mutation.name
                                    ]
                            , width = fill
                            , view = displayMutation modelTable.dataModelWithMutations mutation
                            }
                )
                (fields ++ mutationsWithoutCreateAndUpdate)
        }


removeRightPartOfString : String -> String -> String
removeRightPartOfString strToRemove str =
    String.dropRight (String.length strToRemove) str


displayMutation : DataModelWithMutations -> Mutation -> DataRow -> Element Msg
displayMutation dataModel mutation dataRow =
    let
        id =
            dataRow.row
                |> List.filter (\field -> field.key == "id")
                |> List.head
                |> Maybe.map .value
                |> Maybe.withDefault Nothing
                |> Maybe.map Id
    in
    case removeRightPartOfString dataModel.name mutation.name of
        "destroy" ->
            case id of
                Just id_ ->
                    Input.button
                        [ height <| px 30, centerY ]
                        { label = displayIcon FA.trash
                        , onPress = Just <| Destroy mutation id_ dataModel
                        }

                Nothing ->
                    none

        "update" ->
            case id of
                Just id_ ->
                    Input.button
                        []
                        { label = text mutation.name
                        , onPress = Just <| UpdateMutation mutation id_ dataRow dataModel
                        }

                Nothing ->
                    none

        _ ->
            none


maybeShowCaret : ModelTable -> String -> Element Msg
maybeShowCaret modelTable fieldName =
    if modelTable.orderBy == fieldName then
        if modelTable.orderDirection == Asc then
            Element.el [ Font.size 10, alignRight ] <| text "▲"

        else
            Element.el [ Font.size 10, alignRight ] <| text "▼"

    else
        none



-- Input.text :
--     List (Attribute msg)
--     ->
--         { onChange : String -> msg
--         , text : String
--         , placeholder : Maybe (Placeholder msg)
--         , label : Label msg
--         }
--     -> Element msg


displayData : Field -> String -> DataRow -> Element Msg
displayData field dataModelName data =
    el [ Border.solid, Border.width 1, Font.center ] <|
        case data.row |> List.filter (\d -> d.key == field.name) |> List.head of
            Just d ->
                Input.text
                    [ width fill, height fill ]
                    { onChange = DataCellChanged dataModelName field.name
                    , text = Maybe.withDefault "null" d.value
                    , placeholder = Nothing
                    , label = Input.labelHidden (dataModelName ++ " " ++ field.name)
                    }

            Nothing ->
                none


graphQLTable : Model -> Element Msg
graphQLTable model =
    column
        [ width fill
        , spacing 10
        ]
        [ displayError model.error
        , displaySearchBar model.searchInput
        , row
            [ width fill
            , spacing 10
            ]
            (List.map displayFieldName model.filteredIntrospectionData)
        , displayDataModel model.typeOpened
        , Maybe.withDefault none <| Maybe.map displayPagination model.typeOpened
        ]


displaySearchBar : String -> Element Msg
displaySearchBar searchInput =
    row
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
    option
        [ Html.Attributes.value (String.fromInt v)
        , Html.Attributes.selected (currentPerPage == v)
        ]
        [ Html.text (String.fromInt v) ]


displayPagination : ModelTable -> Element Msg
displayPagination { page, perPage, data } =
    column []
        [ row
            [ width fill
            , spacing 10
            , alignTop
            ]
            [ text <| "Page: " ++ String.fromInt page
            , text <| "Par Page: "
            , html <|
                select
                    [ Html.Events.onInput UpdatePerPage ]
                    ([ 25, 50, 100 ] |> List.map (intToOption perPage))
            , text <| " Total pages: " ++ (String.fromInt <| (data.totalCount // perPage) + 1)
            ]
        , row
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
    column
        [ width fill
        , height fill
        , spacing 10
        , pointer
        , onClick (OpenType modelTable)
        ]
        [ text modelTable.dataModelWithMutations.name
        ]


type alias ModelTable =
    { page : Int
    , perPage : Int
    , orderBy : String
    , orderDirection : OrderDirection
    , dataModelWithMutations : DataModelWithMutations
    , inputs : List Input
    , data : Data
    }


type alias Input =
    { label : String
    , value : String
    , type_ : Type
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
    , dataModelWithMutations = DataModelWithMutations dataModel.name dataModel.fields dataModel.inputFields []
    , inputs = dataModel.inputFields |> List.map (\input -> { label = input.name, value = "", type_ = input.type_ })
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
