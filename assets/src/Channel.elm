module Channel exposing
    ( Effect, none, cmd, join, push
    , Expect, expectWhatever, expectJson
    , EventSub, on
    , Error, Channel
    , document
    , Model, Msg
    )

{-|

@docs Effect, none, cmd, join, push
@docs Expect, expectWhatever, expectJson
@docs EventSub, on
@docs Error, Channel
@docs document

-}

import Browser
import Dict exposing (Dict)
import Html
import Json.Decode as Decode exposing (Decoder, Value)
import Ports
import Set


{-| -}
type Effect msg
    = None
    | Cmd (Cmd msg)
    | Join (Result Value ( Channel, Value ) -> msg) String Value
    | Push (Result Value Value -> msg) Channel String Value


{-| -}
type Error
    = BadBody Decode.Error
    | ChannelError Value


{-| -}
type Channel
    = Channel String


{-| -}
none : Effect msg
none =
    None


{-| -}
cmd : Cmd msg -> Effect msg
cmd =
    Cmd


{-| -}
join : (Result Error ( Channel, Value ) -> msg) -> String -> Value -> Effect msg
join onResult =
    Join
        (\result ->
            onResult <|
                case result of
                    Err value ->
                        Err (ChannelError value)

                    Ok channel ->
                        Ok channel
        )


{-| -}
push :
    { channel : Channel
    , event : String
    , payload : Value
    , expect : Expect msg
    }
    -> Effect msg
push config =
    let
        (Expect toMsg) =
            config.expect
    in
    Push toMsg config.channel config.event config.payload


type Expect msg
    = Expect (Result Value Value -> msg)


expectWhatever : (Result Error () -> msg) -> Expect msg
expectWhatever toMsg =
    let
        onResult result =
            case result of
                Err value ->
                    Err (ChannelError value)

                Ok _ ->
                    Ok ()
    in
    Expect (toMsg << onResult)


expectJson : (Result Error a -> msg) -> Decoder a -> Expect msg
expectJson toMsg aDecoder =
    let
        onResult result =
            case result of
                Err value ->
                    Err (ChannelError value)

                Ok value ->
                    case Decode.decodeValue aDecoder value of
                        Err decodeError ->
                            Err (BadBody decodeError)

                        Ok a ->
                            Ok a
    in
    Expect (toMsg << onResult)


{-| -}
type EventSub msg
    = EventSub Channel String (Value -> msg)


{-| -}
on : Channel -> String -> (Value -> msg) -> EventSub msg
on =
    EventSub


{-| -}
document :
    { init : flags -> ( model, Effect msg )
    , update : msg -> model -> ( model, Effect msg )
    , subscriptions : model -> Sub msg
    , eventSubscriptions : model -> List (EventSub msg)
    , view : model -> Browser.Document msg
    }
    -> Program flags (Model model msg) (Msg msg)
document config =
    Browser.document
        { init = init config.init
        , update =
            \msg model ->
                update config.update msg model
                    |> updateOns config.eventSubscriptions
        , subscriptions = subscriptions config.subscriptions
        , view = view config.view
        }


type alias Model model msg =
    { model : model
    , joins : Dict String (Result Value ( Channel, Value ) -> msg)
    , nextPushId : Int
    , pushes : Dict Int (Result Value Value -> msg)
    , ons : Dict ( String, String ) (List (Value -> msg))
    }


init : (flags -> ( model, Effect msg )) -> flags -> ( Model model msg, Cmd (Msg msg) )
init appInit flags =
    let
        ( model, effect ) =
            appInit flags
    in
    { model = model
    , joins = Dict.empty
    , nextPushId = 0
    , pushes = Dict.empty
    , ons = Dict.empty
    }
        |> perform effect


type Msg msg
    = AppMsg msg
    | JoinError { name : String, payload : Value }
    | JoinOk { name : String, payload : Value }
    | PushError { id : Int, payload : Value }
    | PushOk { id : Int, payload : Value }
    | OnReceive { channel : String, event : String, payload : Value }


update :
    (msg -> model -> ( model, Effect msg ))
    -> Msg msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
update appUpdate msg model =
    case msg of
        AppMsg appMsg ->
            let
                ( appModel, effect ) =
                    appUpdate appMsg model.model
            in
            { model | model = appModel }
                |> perform effect

        JoinError { name, payload } ->
            case Dict.get name model.joins of
                Nothing ->
                    ( model, Cmd.none )

                Just onResult ->
                    let
                        ( appModel, effect ) =
                            appUpdate (onResult (Err payload)) model.model
                    in
                    { model
                        | model = appModel
                        , joins = Dict.remove name model.joins
                    }
                        |> perform effect

        JoinOk { name, payload } ->
            case Dict.get name model.joins of
                Nothing ->
                    ( model, Cmd.none )

                Just onResult ->
                    let
                        ( appModel, effect ) =
                            appUpdate (onResult (Ok ( Channel name, payload ))) model.model
                    in
                    { model
                        | model = appModel
                        , joins = Dict.remove name model.joins
                    }
                        |> perform effect

        PushError { id, payload } ->
            case Dict.get id model.pushes of
                Nothing ->
                    ( model, Cmd.none )

                Just onResult ->
                    let
                        ( appModel, effect ) =
                            appUpdate (onResult (Err payload)) model.model
                    in
                    { model
                        | model = appModel
                        , pushes = Dict.remove id model.pushes
                    }
                        |> perform effect

        PushOk { id, payload } ->
            case Dict.get id model.pushes of
                Nothing ->
                    ( model, Cmd.none )

                Just onResult ->
                    let
                        ( appModel, effect ) =
                            appUpdate (onResult (Ok payload)) model.model
                    in
                    { model
                        | model = appModel
                        , pushes = Dict.remove id model.pushes
                    }
                        |> perform effect

        OnReceive { channel, event, payload } ->
            case Dict.get ( channel, event ) model.ons of
                Nothing ->
                    ( model, Cmd.none )

                Just toValues ->
                    let
                        ( appModel, allEffects ) =
                            List.foldl
                                (\toValue ( nextModel, effects ) ->
                                    appUpdate (toValue payload) nextModel
                                        |> Tuple.mapSecond (\effect -> effect :: effects)
                                )
                                ( model.model, [] )
                                toValues
                    in
                    List.foldl
                        (\effect ( nextModel, nextCmd ) ->
                            nextModel
                                |> perform effect
                                |> Tuple.mapSecond (\cmd_ -> Cmd.batch [ cmd_, nextCmd ])
                        )
                        ( { model | model = appModel }, Cmd.none )
                        allEffects


updateOns :
    (model -> List (EventSub msg))
    -> ( Model model msg, Cmd (Msg msg) )
    -> ( Model model msg, Cmd (Msg msg) )
updateOns eventSubscriptions ( model, cmd_ ) =
    let
        ons =
            List.foldl
                (\(EventSub (Channel channel) name toValue) collected ->
                    Dict.update ( channel, name )
                        (\maybeToValues ->
                            case maybeToValues of
                                Nothing ->
                                    Just [ toValue ]

                                Just toValues ->
                                    Just (toValue :: toValues)
                        )
                        collected
                )
                Dict.empty
                eventSubs

        adds =
            List.foldl
                (\(EventSub (Channel channel) name _) add ->
                    if
                        Dict.member ( channel, name ) ons
                            && not (Dict.member ( channel, name ) model.ons)
                    then
                        { channel = channel
                        , event = name
                        }
                            :: add

                    else
                        add
                )
                []
                eventSubs

        removes =
            Set.toList <|
                Set.diff
                    (Set.fromList (Dict.keys model.ons))
                    (Set.fromList (List.map toKey eventSubs))

        toKey (EventSub (Channel channel) name _) =
            ( channel, name )

        eventSubs =
            eventSubscriptions model.model
    in
    ( { model | ons = ons }
    , Cmd.batch
        [ cmd_
        , adds
            |> List.map Ports.on
            |> Cmd.batch
        , removes
            |> List.map
                (\( channel, name ) ->
                    Ports.off
                        { channel = channel
                        , event = name
                        }
                )
            |> Cmd.batch
        ]
    )


subscriptions : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
subscriptions appSubscriptions model =
    Sub.batch
        [ Sub.map AppMsg (appSubscriptions model.model)
        , Ports.joinError JoinError
        , Ports.joinOk JoinOk
        , Ports.pushError PushError
        , Ports.pushOk PushOk
        , Ports.onReceive OnReceive
        ]


view : (model -> Browser.Document msg) -> Model model msg -> Browser.Document (Msg msg)
view appView model =
    let
        { title, body } =
            appView model.model
    in
    { title = title
    , body = List.map (Html.map AppMsg) body
    }


perform : Effect msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
perform effect model =
    case effect of
        None ->
            ( model, Cmd.none )

        Cmd cmd_ ->
            ( model, Cmd.map AppMsg cmd_ )

        Join onResult name params ->
            ( { model | joins = Dict.insert name onResult model.joins }
            , Ports.join
                { name = name
                , params = params
                }
            )

        Push onResult (Channel channel) event payload ->
            ( { model
                | pushes = Dict.insert model.nextPushId onResult model.pushes
                , nextPushId = model.nextPushId + 1
              }
            , Ports.push
                { id = model.nextPushId
                , channel = channel
                , event = event
                , payload = payload
                }
            )
