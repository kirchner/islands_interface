port module Ports exposing
    ( join
    , joinError
    , joinOk
    , off
    , on
    , onReceive
    , push
    , pushError
    , pushOk
    )

import Json.Encode exposing (Value)


port join : { name : String, params : Value } -> Cmd msg


port joinError : ({ name : String, payload : Value } -> msg) -> Sub msg


port joinOk : ({ name : String, payload : Value } -> msg) -> Sub msg


port push : { id : Int, channel : String, event : String, payload : Value } -> Cmd msg


port pushError : ({ id : Int, payload : Value } -> msg) -> Sub msg


port pushOk : ({ id : Int, payload : Value } -> msg) -> Sub msg


port on : { channel : String, event : String } -> Cmd msg


port onReceive : ({ channel : String, event : String, payload : Value } -> msg) -> Sub msg


port off : { channel : String, event : String } -> Cmd msg
