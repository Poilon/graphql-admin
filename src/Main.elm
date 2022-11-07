module Main exposing (main, view)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Events exposing (onClick)
import Html exposing (Html, button, div)
import Http exposing (Error)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import RemoteData as RemoteData exposing (RemoteData)
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
                    [ ( "query", graphQlQuery )
                    , ( "variables", graphQlVariables )
                    ]
                )
        , expect = Http.expectJson GotResponse dataDecoder
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
    { name : String, kind : Type }


type alias Type =
    { name : Maybe String, kind : String }


graphQlQuery : Value
graphQlQuery =
    Encode.string "query { __schema { types { name fields { name type { name kind } }}}}"


dataDecoder : D.Decoder (List DataModel)
dataDecoder =
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


value : Value
value =
    Encode.object
        [ ( "query", graphQlQuery )
        , ( "variables", graphQlVariables )
        ]


type alias Model =
    { count : Int
    , introspectionData : List DataModel
    , filteredIntrospectionData : List DataModel
    , typeOpened : Maybe DataModel
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
    | GotResponse (Result Error (List DataModel))
    | OpenType DataModel


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

        GotResponse response ->
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

        OpenType dataModel ->
            ( { model | typeOpened = Just dataModel }, Cmd.none )


getDataModelsWithFields : List DataModel -> List String -> List DataModel
getDataModelsWithFields introspectionData dataModels =
    introspectionData
        |> List.filter (\data -> List.member data.name dataModels)


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


displayDataModel : Maybe DataModel -> Element Msg
displayDataModel maybeDataModel =
    case maybeDataModel of
        Just dataModel ->
            Element.column
                [ width fill
                , height fill
                , Element.padding 10
                ]
                [ Element.text dataModel.name
                , displayFields dataModel.fields
                ]

        Nothing ->
            none


displayFields : Maybe (List Field) -> Element Msg
displayFields fields_ =
    case fields_ of
        Just fields ->
            Element.column
                [ width fill
                , height fill
                , Element.padding 10
                ]
                (List.map displayField fields)

        Nothing ->
            Element.text "No fields"


displayField : Field -> Element Msg
displayField field =
    Element.column
        [ width fill
        , height fill
        , Element.padding 10
        ]
        [ Element.text field.name
        ]


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
            , height fill
            , spacing 10
            ]
            (List.map displayFieldName model.filteredIntrospectionData)
        , displayDataModel model.typeOpened
        ]


displayFieldName : DataModel -> Element Msg
displayFieldName dataModel =
    Element.column
        [ width fill
        , height fill
        , spacing 10
        , pointer
        , onClick (OpenType dataModel)
        ]
        [ Element.text dataModel.name
        ]


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



-- PaginatedDataModel
-- DataModel
-- fields qui ont comme type qui ont un kind == SCALAR
