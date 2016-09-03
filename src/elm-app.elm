port module Main exposing (..)

import Html exposing (Html, Attribute, div, hr, p, input, text, span, button, label)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck)
import String
import Array
import Regex
import Json.Encode
import Json.Decode exposing ((:=))


forwardToJsonValue : ForwardConfig -> Json.Encode.Value
forwardToJsonValue { enabled, local, remote } =
    Json.Encode.object
        [ ( "enabled", Json.Encode.bool enabled )
        , ( "local", Json.Encode.string local )
        , ( "remote", Json.Encode.string remote )
        ]


encodeForwards : List ForwardConfig -> String
encodeForwards cfg =
    Json.Encode.encode 0
        (Json.Encode.object
            [ ( "forwards"
              , Json.Encode.list
                    (List.map forwardToJsonValue cfg)
              )
            ]
        )


jsonToForward =
    Json.Decode.object5 ForwardConfig
        ("enabled" := Json.Decode.bool)
        ("local" := Json.Decode.string)
        ("remote" := Json.Decode.string)
        ("lastLocalError" := Json.Decode.string)
        ("lastRemoteError" := Json.Decode.string)


decodeForwards : String -> List ForwardConfig
decodeForwards data =
    case Json.Decode.decodeString ("forwards" := (Json.Decode.list jsonToForward)) data of
        Err msg ->
            []

        Ok f ->
            f


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias ForwardConfig =
    { enabled : Bool
    , local : String
    , remote : String
    , lastLocalError : String
    , lastRemoteError : String
    }


type alias Model =
    { loaded : Bool
    , forwards : List ForwardConfig
    }


init : ( Model, Cmd Msg )
init =
    ( Model False [], loadConfig () )



-- UPDATE


type Msg
    = New
    | Update Int ForwardConfig
    | Remove Int
    | Loaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New ->
            let
                nf =
                    model.forwards ++ [ { enabled = False, local = "", remote = "", lastLocalError = "", lastRemoteError = "" } ]
            in
                ( { model | forwards = nf }
                , saveConfig (encodeForwards nf)
                )

        Update i f ->
            let
                nf =
                    (List.take i model.forwards) ++ [ f ] ++ (List.drop (i + 1) model.forwards)
            in
                ( { model | forwards = nf }
                , saveConfig (encodeForwards nf)
                )

        Remove i ->
            let
                nf =
                    (List.take i model.forwards) ++ (List.drop (i + 1) model.forwards)
            in
                ( { model | forwards = nf }
                , saveConfig (encodeForwards nf)
                )

        Loaded cfgStr ->
            ( { model
                | loaded = True
                , forwards = decodeForwards cfgStr
              }
            , Cmd.none
            )



-- Save/Load


port loadConfig : () -> Cmd msg


port loadedConfig : (String -> msg) -> Sub msg


port saveConfig : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    loadedConfig Loaded



-- VIEW


validAddr : String -> Bool
validAddr addr =
    case String.toInt (Regex.replace Regex.All (Regex.regex ".*:") (\_ -> "") addr) of
        Err msg ->
            False

        Ok val ->
            val >= 1 && val <= 65535


forwardView : Int -> ForwardConfig -> Html Msg
forwardView index fCfg =
    div [ class "form-inline" ]
        [ div [ class "checkbox" ]
            [ label []
                [ input
                    [ type' "checkbox"
                    , disabled (not (validAddr fCfg.local) || not (validAddr fCfg.remote))
                    , onCheck (\checked -> (Update index { fCfg | enabled = checked }))
                    , checked fCfg.enabled
                    ]
                    []
                , text "Active"
                ]
            ]
        , div [ class "form-group" ]
            [ input
                [ class "form-control"
                , placeholder "Local Address"
                , value fCfg.local
                , onInput (\newLocal -> (Update index { fCfg | local = newLocal }))
                ]
                []
            ]
        , div [ class "form-group" ]
            [ input
                [ value fCfg.remote
                , class "form-control"
                , placeholder "Remote Address"
                , onInput (\newRemote -> (Update index { fCfg | remote = newRemote }))
                ]
                []
            ]
        , button [ class "btn btn-danger", onClick (Remove index) ] [ text "Remove" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        ([ p [] [ text "Use the controls below to configure port forwards. When 'Active', the field will turn green when a sucessful connection has been made. A number will also appear on the right during 'Active' mode, indicating the 'current / total' number of connections." ]
         , hr [] []
         ]
            ++ (if not model.loaded then
                    [ p [ class "lead" ] [ text "Loading..." ] ]
                else
                    ((if List.length model.forwards == 0 then
                        [ p [ class "lead text-muted" ] [ text "No configured port forwards" ] ]
                      else
                        List.indexedMap forwardView model.forwards
                     )
                        ++ [ hr [] []
                           , button [ class "btn btn-default", onClick New ] [ text "Add" ]
                           ]
                    )
               )
        )
