module Main exposing (main, view)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http exposing (Error)
import Json.Decode as D exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RemoteData as RemoteData exposing (RemoteData)
import Url


graphQlQuery : Value
graphQlQuery =
    Encode.string "query { __schema { types { name } } }"


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


dataDecoder : D.Decoder (List String)
dataDecoder =
    D.field "data" (D.field "__schema" (D.field "types" (D.list (D.field "name" D.string))))


value : Value
value =
    Encode.object
        [ ( "query", graphQlQuery )
        , ( "variables", graphQlVariables )
        ]


type alias Model =
    { count : Int, types : List String }


type alias Flags =
    {}


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { count = 0, types = [] }, makeRequest )


type Msg
    = Increment
    | Decrement
    | UrlRequested UrlRequest
    | UrlChanged Url.Url
    | GotResponse (Result Error (List String))


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
                Ok typesList ->
                    ( { model
                        | types =
                            typesList
                                |> List.filter (String.startsWith "Paginated")
                                |> List.map (String.dropLeft 9)
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "GRAPHQL ADMIN"
    , body =
        [ div []
            [ button [ onClick Increment ] [ text "+1" ]
            , div [] [ text <| String.fromInt model.count ]
            , button [ onClick Decrement ] [ text "-1" ]
            ]
        ]
    }


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
