port module Main exposing (..)

import Html exposing (Html, Attribute, div, hr, input, text, button)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck)
import String
import Array
import Regex
import Json.Encode
import Json.Decode exposing ((:=))


forwardToJsonValue : ForwardConfig -> Json.Encode.Value
forwardToJsonValue ( enabled, local, remote ) =
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
    Json.Decode.object3 (,,)
        ("enabled" := Json.Decode.bool)
        ("local" := Json.Decode.string)
        ("remote" := Json.Decode.string)


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
    ( Bool, String, String )


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
                    model.forwards ++ [ ( False, "", "" ) ]
            in
                ( { loaded = model.loaded
                  , forwards = nf
                  }
                , saveConfig (encodeForwards nf)
                )

        Update i f ->
            let
                nf =
                    (List.take i model.forwards) ++ [ f ] ++ (List.drop (i + 1) model.forwards)
            in
                ( { loaded = model.loaded
                  , forwards = nf
                  }
                , saveConfig (encodeForwards nf)
                )

        Remove i ->
            let
                nf =
                    (List.take i model.forwards) ++ (List.drop (i + 1) model.forwards)
            in
                ( { loaded = model.loaded
                  , forwards = nf
                  }
                , saveConfig (encodeForwards nf)
                )

        Loaded cfgStr ->
            ( { loaded = True
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
forwardView index ( enabled, local, remote ) =
    div []
        [ input
            [ type' "checkbox"
            , disabled (not (validAddr local) || not (validAddr remote))
            , onCheck (\checked -> (Update index ( checked, local, remote )))
            , checked enabled
            ]
            []
        , input [ value local, onInput (\newLocal -> (Update index ( False, newLocal, remote ))) ] []
        , input [ value remote, onInput (\newRemote -> (Update index ( False, local, newRemote ))) ] []
        , button [ onClick (Remove index) ] [ text "Remove" ]
        ]


view : Model -> Html Msg
view model =
    div []
        (if not model.loaded then
            [ div [ class "disabled" ] [ text "Loading..." ] ]
         else
            ((if List.length model.forwards == 0 then
                [ div [ class "disabled" ] [ text "No configured port forwards" ] ]
              else
                List.indexedMap forwardView model.forwards
             )
                ++ [ hr [] []
                   , button [ onClick New ] [ text "Add" ]
                   ]
            )
        )
