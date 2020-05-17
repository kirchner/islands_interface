module Main exposing (main)

import Browser
import Browser.Events
import Channel exposing (Channel, Effect)
import Element exposing (Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Set
import Ui.Atom.Button as Button exposing (button)
import Ui.Atom.InputText as InputText exposing (inputText)
import Ui.Atom.Tile as Tile exposing (tile)
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography
import Url.Builder


type alias GuessedCoordinate =
    { player : String
    , row : Int
    , col : Int
    , result : GuessResult
    }


type alias GuessResult =
    { hit : Bool
    , island : String
    , win : String
    }


guessedCoordinateDecoder : Decoder GuessedCoordinate
guessedCoordinateDecoder =
    Decode.succeed GuessedCoordinate
        |> Decode.required "player" Decode.string
        |> Decode.required "row" Decode.int
        |> Decode.required "col" Decode.int
        |> Decode.required "result" guessResultDecoder


guessResultDecoder : Decoder GuessResult
guessResultDecoder =
    Decode.succeed GuessResult
        |> Decode.required "hit" Decode.bool
        |> Decode.required "island" Decode.string
        |> Decode.required "win" Decode.string


main : Program Flags (Channel.Model Model Msg) (Channel.Msg Msg)
main =
    Channel.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , eventSubscriptions = eventSubscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { device : Device
    , page : Page
    }


type Page
    = -- HOST ONLY
      CreatingGame
        { host : String
        , name : String
        , channel : Requested Channel
        }
    | WaitingForGuest
        { host : String
        , name : String
        , channel : Channel
        }
      -- GUEST ONLY
    | JoiningGame
        { hostName : String
        , name : String
        , channel : Requested Channel
        }
      -- BOTH
    | PlayersSet PlayersSetData
    | WaitingForOpponent
        { channel : Channel
        , player : Player
        , placedIslands : List ( Island, Coordinate )
        }
    | Playing PlayingData


type Requested a
    = NotAsked
    | Requesting
    | Received a
    | RequestFailed


type alias PlayersSetData =
    { channel : Channel
    , player : Player
    , placedIslands : List ( Island, Coordinate )
    , unplacedIslands : List Island
    , selection : Selection
    , requestedPlacement : Maybe ( Island, Coordinate )
    , opponentReady : Bool
    }


type alias PlayingData =
    { channel : Channel
    , player : Player
    , state : State
    , placedIslands : List ( Island, Coordinate )
    , opponentGuesses : List Coordinate
    , opponentTiles : List ( Coordinate, Bool )
    , forestedIslands : List Island
    }


forestedInfo : List Island -> String
forestedInfo islands =
    case List.reverse islands of
        [] ->
            "No islands forested yet."

        island :: [] ->
            "You have forested the " ++ islandToText island ++ " Island."

        lastIslands :: otherIslands ->
            "You have forested the "
                ++ (List.reverse otherIslands
                        |> List.map islandToText
                        |> String.join ", "
                   )
                ++ ", and "
                ++ islandToText lastIslands
                ++ " Islands."


type Player
    = Host
    | Guest


playerToString : Player -> String
playerToString player =
    case player of
        Host ->
            "player1"

        Guest ->
            "player2"


playerFromString : String -> Maybe Player
playerFromString string =
    case string of
        "player1" ->
            Just Host

        "player2" ->
            Just Guest

        _ ->
            Nothing


type State
    = Guessing
    | Guessed Coordinate
    | OpponentGuessing
    | Won
    | OpponentWon


type Selection
    = None
    | Tile Coordinate
    | Island Island Coordinate


type Island
    = Atoll
    | Dot
    | LShape
    | SShape
    | Square


islandToText : Island -> String
islandToText island =
    case island of
        Atoll ->
            "Atoll"

        Dot ->
            "Dot"

        LShape ->
            "L"

        SShape ->
            "S"

        Square ->
            "Square"


islandToString : Island -> String
islandToString island =
    case island of
        Atoll ->
            "atoll"

        Dot ->
            "dot"

        LShape ->
            "l_shape"

        SShape ->
            "s_shape"

        Square ->
            "square"


islandFromString : String -> Maybe Island
islandFromString string =
    case string of
        "atoll" ->
            Just Atoll

        "dot" ->
            Just Dot

        "l_shape" ->
            Just LShape

        "s_shape" ->
            Just SShape

        "square" ->
            Just Square

        _ ->
            Nothing


allIslands : List Island
allIslands =
    [ Atoll, Dot, LShape, SShape, Square ]


offsets : Island -> List Coordinate
offsets island =
    case island of
        Atoll ->
            [ Coordinate 0 0, Coordinate 0 1, Coordinate 1 1, Coordinate 2 0, Coordinate 2 1 ]

        Dot ->
            [ Coordinate 0 0 ]

        LShape ->
            [ Coordinate 0 0, Coordinate 1 0, Coordinate 2 0, Coordinate 2 1 ]

        SShape ->
            [ Coordinate 0 1, Coordinate 0 2, Coordinate 1 0, Coordinate 1 1 ]

        Square ->
            [ Coordinate 0 0, Coordinate 0 1, Coordinate 1 0, Coordinate 1 1 ]


type alias Coordinate =
    { row : Int
    , col : Int
    }


minRange : Int
minRange =
    1


maxRange : Int
maxRange =
    10


type alias Flags =
    { hash : String
    , protocol : String
    , host : String
    , innerWidth : Int
    , innerHeight : Int
    }


init : Flags -> ( Model, Effect Msg )
init flags =
    let
        page =
            if flags.hash == "" then
                CreatingGame
                    { host = flags.protocol ++ "//" ++ flags.host
                    , name = ""
                    , channel = NotAsked
                    }

            else
                JoiningGame
                    { hostName = String.dropLeft 1 flags.hash
                    , name = ""
                    , channel = NotAsked
                    }
    in
    ( { device =
            Element.classifyDevice
                { width = flags.innerWidth
                , height = flags.innerHeight
                }
      , page = page
      }
    , Channel.none
    )



-- UPDATE


type Msg
    = ResizedViewport Int Int
    | UserChangedName String
    | UserPressedCreateGame
    | ReceivedJoinResult (Result Channel.Error ( Channel, Value ))
    | ReceivedNewGameResponse (Result Channel.Error ())
    | UserPressedJoinTheGame
    | ReceivedAddPlayerResult (Result Channel.Error ())
    | ReceivedPlayerAdded Value
      --
    | UserPressedTile Coordinate
    | UserPressedIslandTile Island Coordinate
    | ReceivedPositionIslandResult Island Int Int (Result Channel.Error ())
    | ReceivedPlayerSetIslands Value
    | UserPressedImReady
    | ReceivedSetIslandResult (Result Channel.Error ())
      --
    | UserPressedOpponentTile Coordinate
    | ReceivedGuessCoordinateResult (Result Channel.Error ())
    | ReceivedPlayerGuessedCoordinate Value


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case ( msg, model.page ) of
        ( ResizedViewport width height, _ ) ->
            ( { model
                | device =
                    Element.classifyDevice
                        { width = width
                        , height = height
                        }
              }
            , Channel.none
            )

        -- ONLY HOST
        ( UserChangedName name, CreatingGame data ) ->
            ( { model | page = CreatingGame { data | name = name } }
            , Channel.none
            )

        ( UserPressedCreateGame, CreatingGame { host, name } ) ->
            if name == "" then
                ( model, Channel.none )

            else
                ( { model
                    | page =
                        CreatingGame
                            { host = host
                            , name = name
                            , channel = Requesting
                            }
                  }
                , Channel.join ReceivedJoinResult
                    ("game:" ++ name)
                    (Encode.object
                        [ ( "screen_name", Encode.string name ) ]
                    )
                )

        ( ReceivedJoinResult result, CreatingGame { host, name } ) ->
            case result of
                Err _ ->
                    ( { model
                        | page =
                            CreatingGame
                                { host = host
                                , name = name
                                , channel = RequestFailed
                                }
                      }
                    , Channel.none
                    )

                Ok ( channel, _ ) ->
                    ( { model
                        | page =
                            CreatingGame
                                { host = host
                                , name = name
                                , channel = Received channel
                                }
                      }
                    , Channel.push
                        { channel = channel
                        , event = "new_game"
                        , expect = Channel.expectWhatever ReceivedNewGameResponse
                        , payload = Encode.null
                        }
                    )

        ( ReceivedNewGameResponse result, CreatingGame data ) ->
            case result of
                Err _ ->
                    ( model, Channel.none )

                Ok _ ->
                    case data.channel of
                        Received channel ->
                            ( { model
                                | page =
                                    WaitingForGuest
                                        { host = data.host
                                        , name = data.name
                                        , channel = channel
                                        }
                              }
                            , Channel.none
                            )

                        _ ->
                            ( model, Channel.none )

        ( _, CreatingGame _ ) ->
            ( model, Channel.none )

        ( ReceivedPlayerAdded _, WaitingForGuest { channel } ) ->
            ( { model | page = PlayersSet (initPlayersSet channel Host) }
            , Channel.none
            )

        ( _, WaitingForGuest _ ) ->
            ( model, Channel.none )

        -- ONLY GUEST
        ( UserChangedName name, JoiningGame data ) ->
            ( { model | page = JoiningGame { data | name = name } }
            , Channel.none
            )

        ( UserPressedJoinTheGame, JoiningGame { hostName, name } ) ->
            ( { model
                | page =
                    JoiningGame
                        { hostName = hostName
                        , name = name
                        , channel = Requesting
                        }
              }
            , Channel.join ReceivedJoinResult
                ("game:" ++ hostName)
                (Encode.object
                    [ ( "screen_name", Encode.string name ) ]
                )
            )

        ( ReceivedJoinResult result, JoiningGame data ) ->
            case result of
                Err _ ->
                    ( model, Channel.none )

                Ok ( channel, _ ) ->
                    ( { model | page = JoiningGame { data | channel = Received channel } }
                    , Channel.push
                        { channel = channel
                        , event = "add_player"
                        , expect = Channel.expectWhatever ReceivedAddPlayerResult
                        , payload = Encode.string data.name
                        }
                    )

        ( ReceivedAddPlayerResult _, JoiningGame _ ) ->
            ( model, Channel.none )

        ( ReceivedPlayerAdded _, JoiningGame data ) ->
            case data.channel of
                Received channel ->
                    ( { model | page = PlayersSet (initPlayersSet channel Guest) }
                    , Channel.none
                    )

                _ ->
                    ( model, Channel.none )

        ( _, JoiningGame _ ) ->
            ( model, Channel.none )

        -- BOTH
        ( UserPressedTile coordinate, PlayersSet data ) ->
            case data.selection of
                None ->
                    ( { model | page = PlayersSet { data | selection = Tile coordinate } }
                    , Channel.none
                    )

                Tile _ ->
                    ( { model | page = PlayersSet { data | selection = Tile coordinate } }
                    , Channel.none
                    )

                Island island offset ->
                    let
                        ( newData, effect ) =
                            requestPositionIsland coordinate island offset data
                    in
                    ( { model | page = PlayersSet newData }
                    , effect
                    )

        ( UserPressedIslandTile island offset, PlayersSet data ) ->
            case data.selection of
                None ->
                    ( { model | page = PlayersSet { data | selection = Island island offset } }
                    , Channel.none
                    )

                Tile coordinate ->
                    let
                        ( newData, cmd ) =
                            requestPositionIsland coordinate island offset data
                    in
                    ( { model | page = PlayersSet newData }
                    , cmd
                    )

                Island _ _ ->
                    ( { model | page = PlayersSet { data | selection = Island island offset } }
                    , Channel.none
                    )

        ( ReceivedPositionIslandResult island row col result, PlayersSet data ) ->
            case data.requestedPlacement of
                Nothing ->
                    ( model, Channel.none )

                Just ( requestedIsland, coordinate ) ->
                    if island == requestedIsland && Coordinate row col == coordinate then
                        case result of
                            Err _ ->
                                ( { model
                                    | page =
                                        PlayersSet
                                            { data
                                                | selection = None
                                                , requestedPlacement = Nothing
                                            }
                                  }
                                , Channel.none
                                )

                            Ok () ->
                                ( { model
                                    | page = PlayersSet (addToPlacedIslands island row col data)
                                  }
                                , Channel.none
                                )

                    else
                        ( model, Channel.none )

        ( ReceivedPlayerSetIslands value, PlayersSet data ) ->
            if opponentSetIslands data.player value then
                ( { model | page = PlayersSet { data | opponentReady = True } }
                , Channel.none
                )

            else
                ( model, Channel.none )

        ( UserPressedImReady, PlayersSet data ) ->
            ( { model
                | page =
                    if data.opponentReady then
                        Playing (initPlaying data.channel data.player data.placedIslands)

                    else
                        WaitingForOpponent
                            { channel = data.channel
                            , player = data.player
                            , placedIslands = data.placedIslands
                            }
              }
            , Channel.push
                { channel = data.channel
                , event = "set_islands"
                , expect = Channel.expectWhatever ReceivedSetIslandResult
                , payload = Encode.string (playerToString data.player)
                }
            )

        ( _, PlayersSet _ ) ->
            ( model, Channel.none )

        -- WAITING FOR OPPONENT
        ( ReceivedPlayerSetIslands value, WaitingForOpponent data ) ->
            if opponentSetIslands data.player value then
                ( { model
                    | page =
                        Playing
                            (initPlaying data.channel
                                data.player
                                data.placedIslands
                            )
                  }
                , Channel.none
                )

            else
                ( model, Channel.none )

        ( _, WaitingForOpponent _ ) ->
            ( model, Channel.none )

        -- PLAYING
        ( _, Playing data ) ->
            let
                ( newData, cmd ) =
                    updatePlaying msg data
            in
            ( { model | page = Playing newData }
            , cmd
            )


initPlayersSet : Channel -> Player -> PlayersSetData
initPlayersSet channel player =
    { channel = channel
    , player = player
    , placedIslands = []
    , unplacedIslands = allIslands
    , selection = None
    , requestedPlacement = Nothing
    , opponentReady = False
    }


requestPositionIsland :
    Coordinate
    -> Island
    -> Coordinate
    -> PlayersSetData
    -> ( PlayersSetData, Effect Msg )
requestPositionIsland coordinate island offset data =
    let
        requestedCoordinate =
            { row = coordinate.row - offset.row
            , col = coordinate.col - offset.col
            }
    in
    ( { data
        | selection = None
        , requestedPlacement = Just ( island, requestedCoordinate )
      }
    , Channel.push
        { channel = data.channel
        , event = "position_island"
        , expect =
            Channel.expectWhatever
                (ReceivedPositionIslandResult island
                    requestedCoordinate.row
                    requestedCoordinate.col
                )
        , payload =
            Encode.object
                [ ( "player", Encode.string (playerToString data.player) )
                , ( "island", Encode.string (islandToString island) )
                , ( "row", Encode.int requestedCoordinate.row )
                , ( "col", Encode.int requestedCoordinate.col )
                ]
        }
    )


opponentSetIslands : Player -> Value -> Bool
opponentSetIslands player value =
    case Decode.decodeValue playerSetIslandDecoder value of
        Err _ ->
            False

        Ok opponent ->
            opponent /= player


addToPlacedIslands : Island -> Int -> Int -> PlayersSetData -> PlayersSetData
addToPlacedIslands island row col data =
    { data
        | selection = None
        , placedIslands =
            ( island
            , { row = row
              , col = col
              }
            )
                :: data.placedIslands
        , unplacedIslands =
            List.filter
                (\unplacedIsland -> unplacedIsland /= island)
                data.unplacedIslands
        , requestedPlacement = Nothing
    }


initPlaying : Channel -> Player -> List ( Island, Coordinate ) -> PlayingData
initPlaying channel player placedIslands =
    { channel = channel
    , player = player
    , state =
        case player of
            Host ->
                Guessing

            Guest ->
                OpponentGuessing
    , placedIslands = placedIslands
    , opponentGuesses = []
    , opponentTiles = []
    , forestedIslands = []
    }


updatePlaying : Msg -> PlayingData -> ( PlayingData, Effect Msg )
updatePlaying msg data =
    case msg of
        UserPressedOpponentTile coordinate ->
            case data.state of
                Guessing ->
                    ( { data | state = Guessed coordinate }
                    , Channel.push
                        { channel = data.channel
                        , event = "guess_coordinate"
                        , expect = Channel.expectWhatever ReceivedGuessCoordinateResult
                        , payload =
                            Encode.object
                                [ ( "player", Encode.string (playerToString data.player) )
                                , ( "row", Encode.int coordinate.row )
                                , ( "col", Encode.int coordinate.col )
                                ]
                        }
                    )

                _ ->
                    ( data, Channel.none )

        ReceivedGuessCoordinateResult _ ->
            ( data, Channel.none )

        ReceivedPlayerGuessedCoordinate value ->
            case Decode.decodeValue guessedCoordinateDecoder value of
                Err _ ->
                    ( data, Channel.none )

                Ok { player, row, col, result } ->
                    case ( data.state, playerFromString player ) of
                        ( Guessed coordinate, Just guessingPlayer ) ->
                            if guessingPlayer == data.player && Coordinate row col == coordinate then
                                ( { data
                                    | state =
                                        if result.win == "win" then
                                            Won

                                        else
                                            OpponentGuessing
                                    , opponentTiles =
                                        ( coordinate, result.hit ) :: data.opponentTiles
                                    , forestedIslands =
                                        case islandFromString result.island of
                                            Nothing ->
                                                data.forestedIslands

                                            Just island ->
                                                island :: data.forestedIslands
                                  }
                                , Channel.none
                                )

                            else
                                ( data, Channel.none )

                        ( OpponentGuessing, Just guessingPlayer ) ->
                            if guessingPlayer /= data.player then
                                ( { data
                                    | state =
                                        if result.win == "win" then
                                            OpponentWon

                                        else
                                            Guessing
                                    , opponentGuesses = Coordinate row col :: data.opponentGuesses
                                  }
                                , Channel.none
                                )

                            else
                                ( data, Channel.none )

                        _ ->
                            ( data, Channel.none )

        _ ->
            ( data, Channel.none )


playerSetIslandDecoder : Decoder Player
playerSetIslandDecoder =
    Decode.field "player" Decode.string
        |> Decode.andThen
            (\rawPlayer ->
                case playerFromString rawPlayer of
                    Nothing ->
                        Decode.fail ("not a valid player: " ++ rawPlayer)

                    Just player ->
                        Decode.succeed player
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize ResizedViewport


eventSubscriptions : Model -> List (Channel.EventSub Msg)
eventSubscriptions model =
    case model.page of
        CreatingGame data ->
            case data.channel of
                Received channel ->
                    [ Channel.on channel "player_added" ReceivedPlayerAdded ]

                _ ->
                    []

        WaitingForGuest { channel } ->
            [ Channel.on channel "player_added" ReceivedPlayerAdded ]

        JoiningGame data ->
            case data.channel of
                Received channel ->
                    [ Channel.on channel "player_added" ReceivedPlayerAdded ]

                _ ->
                    []

        PlayersSet { channel } ->
            [ Channel.on channel "player_set_islands" ReceivedPlayerSetIslands ]

        WaitingForOpponent { channel } ->
            [ Channel.on channel "player_set_islands" ReceivedPlayerSetIslands ]

        Playing data ->
            case data.state of
                Guessed _ ->
                    [ Channel.on data.channel
                        "player_guessed_coordinate"
                        ReceivedPlayerGuessedCoordinate
                    ]

                OpponentGuessing ->
                    [ Channel.on data.channel
                        "player_guessed_coordinate"
                        ReceivedPlayerGuessedCoordinate
                    ]

                _ ->
                    []



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Islands"
    , body =
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow =
                        Just
                            { color = Ui.Theme.Color.brownLight
                            , offset = ( 0, 0 )
                            , blur = 1
                            , size = 3
                            }
                    }
                ]
            }
            [ Background.color Ui.Theme.Color.background
            , Element.scrollbarY
            , Element.width Element.fill
            , Element.height Element.fill
            , Font.family
                [ Font.typeface "Rubik"
                , Font.sansSerif
                ]
            , Font.size 24
            ]
            (case model.page of
                CreatingGame data ->
                    case data.channel of
                        NotAsked ->
                            viewLobby data.name

                        Requesting ->
                            viewWaitingForGuest data.host data.name

                        Received _ ->
                            viewWaitingForGuest data.host data.name

                        RequestFailed ->
                            Element.text "Request failed"

                WaitingForGuest { host, name } ->
                    viewWaitingForGuest host name

                JoiningGame { hostName, name } ->
                    viewJoiningHost hostName name

                PlayersSet { placedIslands, unplacedIslands, selection, opponentReady } ->
                    viewPlayersSet model.device placedIslands unplacedIslands selection opponentReady

                WaitingForOpponent { placedIslands } ->
                    viewWaitingForOpponent model.device placedIslands

                Playing data ->
                    viewPlaying model.device data
            )
        ]
    }


viewLobby : String -> Element Msg
viewLobby name =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum 480)
        , Element.paddingXY Ui.Theme.Spacing.level1 Ui.Theme.Spacing.level5
        , Element.spacing Ui.Theme.Spacing.level3
        , Region.mainContent
        ]
        [ Ui.Theme.Typography.heading "Welcome to Islands!"
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level2
            , Element.width Element.fill
            ]
            [ inputText UserChangedName UserPressedCreateGame "Pick a name" name
                |> InputText.focusedOnLoad
                |> InputText.toElement
            , button UserPressedCreateGame "Create game"
                |> Button.toElement
            ]
        ]


viewWaitingForGuest : String -> String -> Element Msg
viewWaitingForGuest host name =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum 480)
        , Element.paddingXY Ui.Theme.Spacing.level1 Ui.Theme.Spacing.level5
        , Element.spacing Ui.Theme.Spacing.level3
        , Region.mainContent
        ]
        [ Ui.Theme.Typography.heading ("Hi " ++ name ++ "!")
        , Element.newTabLink
            [ Element.centerX
            , Font.color Ui.Theme.Color.orange
            , Font.underline
            ]
            { url =
                Url.Builder.custom (Url.Builder.CrossOrigin host) [] [] <|
                    Just name
            , label = Element.text (host ++ "/#" ++ name)
            }
        , Element.column
            [ Element.width Element.fill
            , Element.spacing Ui.Theme.Spacing.level1
            ]
            [ Element.el
                [ Element.centerX ]
                (Element.text "Send the above link to a friend.")
            , Element.el
                [ Element.centerX ]
                (Element.text "The game will start as soon as they join.")
            ]
        ]


viewJoiningHost : String -> String -> Element Msg
viewJoiningHost hostName name =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum 480)
        , Element.paddingXY Ui.Theme.Spacing.level1 Ui.Theme.Spacing.level5
        , Element.spacing Ui.Theme.Spacing.level3
        , Region.mainContent
        ]
        [ Ui.Theme.Typography.heading "Welcome to Island!"
        , Element.el
            [ Element.centerX ]
            (Element.text (hostName ++ " is inviting you to play."))
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level2
            , Element.width Element.fill
            ]
            [ inputText UserChangedName UserPressedJoinTheGame "Pick a name" name
                |> InputText.focusedOnLoad
                |> InputText.toElement
            , button UserPressedJoinTheGame "Join the game"
                |> Button.focusedOnLoad
                |> Button.toElement
            ]
        ]


viewPlayersSet : Device -> List ( Island, Coordinate ) -> List Island -> Selection -> Bool -> Element Msg
viewPlayersSet device placedIslands unplacedIslands selection opponentReady =
    Element.column
        [ Element.centerX
        , Element.paddingXY 0 Ui.Theme.Spacing.level3
        , Element.spacing Ui.Theme.Spacing.level3
        , Region.mainContent
        ]
        [ Ui.Theme.Typography.heading "Place your Islands!"
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level3 ]
            [ viewField device (viewSetupTile placedIslands selection)
            , if List.isEmpty unplacedIslands then
                button UserPressedImReady "I'm ready!"
                    |> Button.toElement

              else
                Element.wrappedRow
                    [ Element.spacing Ui.Theme.Spacing.level2
                    , Element.centerX
                    , Element.padding Ui.Theme.Spacing.level1
                    ]
                    (List.map (viewIsland device selection) unplacedIslands)
            , if opponentReady then
                Element.el
                    [ Element.centerX ]
                    (Element.text "The other player is ready.")

              else
                Element.none
            ]
        ]


viewWaitingForOpponent : Device -> List ( Island, Coordinate ) -> Element Msg
viewWaitingForOpponent device placedIslands =
    Element.column
        [ Element.centerX
        , Element.paddingXY 0 Ui.Theme.Spacing.level3
        , Element.spacing Ui.Theme.Spacing.level3
        , Region.mainContent
        ]
        [ Ui.Theme.Typography.heading "Place your Islands!"
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level3 ]
            [ viewField device (viewSetupTile placedIslands None)
            , Element.el
                [ Element.centerX ]
                (Element.text "Waiting for other player.")
            ]
        ]


viewPlaying : Device -> PlayingData -> Element Msg
viewPlaying device data =
    Element.column
        [ Element.centerX
        , Element.paddingXY 0 Ui.Theme.Spacing.level3
        , Element.spacing Ui.Theme.Spacing.level3
        , Region.mainContent
        ]
        [ Element.wrappedRow
            [ Element.spacing Ui.Theme.Spacing.level5
            ]
            [ Element.el
                [ Element.width Element.fill ]
                (Element.column
                    [ Element.spacing Ui.Theme.Spacing.level3
                    , Element.centerX
                    , Element.alignTop
                    ]
                    [ Ui.Theme.Typography.heading "Opponent's sea"
                    , viewField device
                        (viewOpponentTile data.opponentTiles)
                    , Element.paragraph []
                        [ Element.text (forestedInfo data.forestedIslands) ]
                    , case data.state of
                        Guessing ->
                            Element.paragraph
                                [ Element.width Element.fill
                                , Font.bold
                                ]
                                [ Element.text "It's your turn, forest an opponent's tile!" ]

                        _ ->
                            Element.none
                    ]
                )
            , Element.column
                [ Element.spacing Ui.Theme.Spacing.level3
                , Element.centerX
                , Element.alignTop
                ]
                [ Ui.Theme.Typography.heading "Your sea"
                , viewField device (viewYourTile data.placedIslands data.opponentGuesses)
                , case data.state of
                    OpponentGuessing ->
                        Element.el
                            [ Element.centerX
                            , Font.bold
                            ]
                            (Element.text "The other player is foresting now.")

                    _ ->
                        Element.none
                ]
            ]
        , case data.state of
            Won ->
                Element.el
                    [ Element.centerX
                    , Font.bold
                    ]
                    (Element.text "Congratulations! You have won the game!")

            OpponentWon ->
                Element.el
                    [ Element.centerX
                    , Font.bold
                    ]
                    (Element.text "Sorry, the other player has forested all your islands and wins the game.")

            _ ->
                Element.none
        ]



-- VIEW ISLAND


viewIsland : Device -> Selection -> Island -> Element Msg
viewIsland device selection island =
    Element.el
        [ Element.width Element.fill ]
        (Element.column
            [ Element.spacing Ui.Theme.Spacing.level1
            , Element.centerX
            ]
            (List.map (viewIslandTileRow device selection island) (List.range 0 2))
        )


viewIslandTileRow : Device -> Selection -> Island -> Int -> Element Msg
viewIslandTileRow device selection island row =
    Element.row
        [ Element.spacing Ui.Theme.Spacing.level1 ]
        (List.map (viewIslandTile device selection island row) (List.range 0 2))


viewIslandTile : Device -> Selection -> Island -> Int -> Int -> Element Msg
viewIslandTile device selection island row col =
    let
        offsetSet =
            offsets island
                |> List.map (\coordinate -> ( coordinate.row, coordinate.col ))
                |> Set.fromList

        selected =
            case selection of
                Island selectedIsland selectedOffset ->
                    selectedIsland == island && selectedOffset == Coordinate row col

                _ ->
                    False
    in
    if Set.member ( row, col ) offsetSet then
        Element.el
            [ Element.inFront <|
                if selected then
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        , Element.width (Element.px Ui.Theme.Spacing.level3)
                        , Element.height (Element.px Ui.Theme.Spacing.level3)
                        , Border.rounded 3
                        , Background.color Ui.Theme.Color.orange
                        ]
                        Element.none

                else
                    Element.none
            ]
            (tile device
                |> Tile.withRole Tile.Island
                |> Tile.enabledWith (UserPressedIslandTile island (Coordinate row col))
                |> Tile.toElement
            )

    else
        tile device
            |> Tile.toElement



-- VIEW TILE


viewSetupTile : List ( Island, Coordinate ) -> Selection -> Device -> Int -> Int -> Element Msg
viewSetupTile placedIslands selection device row col =
    let
        selected =
            case selection of
                Tile coordinate ->
                    coordinate == Coordinate row col

                _ ->
                    False

        role =
            if List.any containsTile placedIslands then
                Tile.Island

            else
                Tile.Water

        containsTile ( island, islandCoordinate ) =
            offsets island
                |> List.map
                    (\coordinate ->
                        ( islandCoordinate.row + coordinate.row
                        , islandCoordinate.col + coordinate.col
                        )
                    )
                |> Set.fromList
                |> Set.member ( row, col )
    in
    Element.el
        [ Element.inFront <|
            if selected then
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    , Element.width (Element.px Ui.Theme.Spacing.level3)
                    , Element.height (Element.px Ui.Theme.Spacing.level3)
                    , Border.rounded 3
                    , Background.color Ui.Theme.Color.orange
                    ]
                    Element.none

            else
                Element.none
        ]
        (tile device
            |> Tile.withRole role
            |> Tile.enabledWith (UserPressedTile (Coordinate row col))
            |> Tile.toElement
        )


viewOpponentTile : List ( Coordinate, Bool ) -> Device -> Int -> Int -> Element Msg
viewOpponentTile opponentTiles device row col =
    let
        opponentTile ( coordinate, forested ) =
            if Coordinate row col == coordinate then
                Just forested

            else
                Nothing
    in
    case List.head (List.filterMap opponentTile opponentTiles) of
        Nothing ->
            tile device
                |> Tile.withRole Tile.Unknown
                |> Tile.enabledWith (UserPressedOpponentTile (Coordinate row col))
                |> Tile.toElement

        Just False ->
            tile device
                |> Tile.withRole Tile.DiscoveredWater
                |> Tile.toElement

        Just True ->
            tile device
                |> Tile.withRole Tile.Forest
                |> Tile.toElement


viewYourTile :
    List ( Island, Coordinate )
    -> List Coordinate
    -> Device
    -> Int
    -> Int
    -> Element Msg
viewYourTile placedIslands opponentGuesses device row col =
    let
        role =
            if List.any containsTile placedIslands then
                if isGuessed then
                    Tile.Forest

                else
                    Tile.Island

            else if isGuessed then
                Tile.DiscoveredWater

            else
                Tile.Water

        containsTile ( island, islandCoordinate ) =
            offsets island
                |> List.map
                    (\coordinate ->
                        ( islandCoordinate.row + coordinate.row
                        , islandCoordinate.col + coordinate.col
                        )
                    )
                |> Set.fromList
                |> Set.member ( row, col )

        isGuessed =
            List.any (\coordinate -> coordinate == Coordinate row col) opponentGuesses
    in
    tile device
        |> Tile.withRole role
        |> Tile.toElement



-- VIEW FIELD


viewField : Device -> (Device -> Int -> Int -> Element msg) -> Element msg
viewField device viewTile =
    let
        viewRow row =
            Element.row
                [ Element.spacing Ui.Theme.Spacing.level1 ]
                (List.map (viewTile device row) (List.range minRange maxRange))
    in
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level1
        , Element.centerX
        ]
        (List.map viewRow (List.range minRange maxRange))
