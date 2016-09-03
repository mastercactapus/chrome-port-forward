port module Main exposing (..)

import Html exposing (Html, Attribute, div, hr, p, input, text, span, button, label)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck)
import String
import Array exposing (Array)
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


type alias CfgStore =
    { enabled : Bool
    , local : String
    , remote : String
    }


jsonToForward =
    Json.Decode.object3 CfgStore
        ("enabled" := Json.Decode.bool)
        ("local" := Json.Decode.string)
        ("remote" := Json.Decode.string)


cfgDefaults { enabled, local, remote } =
    { enabled = enabled
    , local = local
    , remote = remote
    , lastLocalError = ""
    , lastRemoteError = ""
    , count = 0
    , total = 0
    }


decodeForwards : String -> List ForwardConfig
decodeForwards data =
    case Json.Decode.decodeString ("forwards" := (Json.Decode.list jsonToForward)) data of
        Err msg ->
            []

        Ok f ->
            List.map cfgDefaults f


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias ForwardConfig =
    { enabled : Bool
    , local : String
    , remote : String
    , lastLocalError : String
    , lastRemoteError : String
    , count : Int
    , total : Int
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
    | ListenError ( Int, String )
    | ConnectError ( Int, String )
    | ConnectionCount ( Int, Int, Int )


replaceOne list index item =
    (List.take index list) ++ [ item ] ++ (List.drop (index + 1) list)


replaceLocalErr f err =
    { f | lastLocalError = err }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ListenError ( index, err ) ->
            let
                rec =
                    Array.get index (Array.fromList model.forwards)
            in
                case rec of
                    Nothing ->
                        ( model, Cmd.none )

                    Just r ->
                        ( { model | forwards = replaceOne model.forwards index { r | lastLocalError = err } }
                        , Cmd.none
                        )

        ConnectError ( index, err ) ->
            let
                rec =
                    Array.get index (Array.fromList model.forwards)
            in
                case rec of
                    Nothing ->
                        ( model, Cmd.none )

                    Just r ->
                        ( { model | forwards = replaceOne model.forwards index { r | lastRemoteError = err } }
                        , Cmd.none
                        )

        ConnectionCount ( index, cur, tot ) ->
            let
                rec =
                    Array.get index (Array.fromList model.forwards)
            in
                case rec of
                    Nothing ->
                        ( model, Cmd.none )

                    Just r ->
                        ( { model | forwards = replaceOne model.forwards index { r | count = cur, total = tot, lastRemoteError = "" } }
                        , Cmd.none
                        )

        New ->
            let
                nf =
                    model.forwards ++ [ { enabled = False, local = "", remote = "", lastLocalError = "", lastRemoteError = "", count = 0, total = 0 } ]
            in
                ( { model | forwards = nf }
                , saveConfig (encodeForwards nf)
                )

        Update i f ->
            let
                nf =
                    replaceOne model.forwards i f
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


port countUpdate : (( Int, Int, Int ) -> msg) -> Sub msg


port listenError : (( Int, String ) -> msg) -> Sub msg


port connectError : (( Int, String ) -> msg) -> Sub msg


port saveConfig : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loadedConfig Loaded
        , countUpdate ConnectionCount
        , listenError ListenError
        , connectError ConnectError
        ]



-- VIEW


validAddr : String -> Bool
validAddr addr =
    case String.toInt (Regex.replace Regex.All (Regex.regex ".*:") (\_ -> "") addr) of
        Err msg ->
            False

        Ok val ->
            val >= 1 && val <= 65535


localError : ForwardConfig -> String
localError fCfg =
    if not (validAddr fCfg.local) then
        "missing valid port"
    else if fCfg.enabled then
        fCfg.lastLocalError
    else
        ""


remoteError : ForwardConfig -> String
remoteError fCfg =
    if not (validAddr fCfg.remote) then
        "missing valid port"
    else if fCfg.enabled then
        fCfg.lastRemoteError
    else
        ""


hasLocalError : ForwardConfig -> Bool
hasLocalError fCfg =
    localError fCfg /= ""


hasRemoteError : ForwardConfig -> Bool
hasRemoteError fCfg =
    remoteError fCfg /= ""


forwardView : Int -> ForwardConfig -> Html Msg
forwardView index fCfg =
    div [ class "form-inline" ]
        ([ div [ class "checkbox" ]
            [ label []
                [ input
                    [ type' "checkbox"
                    , disabled (not (validAddr fCfg.local) || not (validAddr fCfg.remote))
                    , onCheck (\checked -> (Update index { fCfg | enabled = checked, count = 0, total = 0, lastLocalError = "", lastRemoteError = "" }))
                    , checked fCfg.enabled
                    ]
                    []
                , text "Active"
                ]
            ]
         , div
            [ class
                (if (hasLocalError fCfg) then
                    "form-group has-error"
                 else
                    "form-group"
                )
            ]
            [ input
                [ class "form-control"
                , placeholder "Local Address"
                , value fCfg.local
                , onInput (\newLocal -> (Update index { fCfg | local = newLocal, enabled = False }))
                ]
                []
            , span [ class "help-block" ] [ text (localError fCfg) ]
            ]
         , div
            [ class
                (if (hasRemoteError fCfg) then
                    "form-group has-error"
                 else
                    "form-group"
                )
            ]
            [ input
                [ value fCfg.remote
                , class "form-control"
                , placeholder "Remote Address"
                , onInput (\newRemote -> (Update index { fCfg | remote = newRemote, enabled = False }))
                ]
                []
            , span [ class "help-block" ] [ text (remoteError fCfg) ]
            ]
         , button [ class "btn btn-danger", onClick (Remove index) ] [ text "Remove" ]
         ]
            ++ if fCfg.enabled then
                [ span [ class "badge" ] [ text ((toString fCfg.count) ++ " / " ++ (toString fCfg.total)) ] ]
               else
                []
        )


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
