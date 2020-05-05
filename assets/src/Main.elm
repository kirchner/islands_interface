port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (Color, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Events
import Json.Decode as Decode
import Set
import Task
import Ui.Atom.Button as Button exposing (button)
import Ui.Atom.Tile as Tile exposing (tile)
import Ui.Theme.Color
import Ui.Theme.Spacing
import Url.Builder


port createGame : String -> Cmd msg


port joinGame : { hostName : String, name : String } -> Cmd msg


port leave : () -> Cmd msg


port playerAdded : (() -> msg) -> Sub msg


port positionIsland : { island : String, row : Int, col : Int } -> Cmd msg


port positionedIsland : ({ island : String, row : Int, col : Int } -> msg) -> Sub msg


port failedPositioningIsland : ({ island : String, row : Int, col : Int } -> msg) -> Sub msg


port setIslands : () -> Cmd msg


port receivedBoard : (() -> msg) -> Sub msg


port failedSettingIslands : (() -> msg) -> Sub msg


port opponentSetIslands : (() -> msg) -> Sub msg


port guessCoordinate : { player : String, row : Int, col : Int } -> Cmd msg


port failedGuessingCoordinate : (() -> msg) -> Sub msg


port playerGuessedCoordinate : (GuessedCoordinate -> msg) -> Sub msg


type alias GuessedCoordinate =
    { player : String
    , row : Int
    , col : Int
    , result :
        { hit : Bool
        , island : String
        , win : String
        }
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type Model
    = GettingDevice Flags
    | -- HOST ONLY
      Lobby
        { host : String
        , name : String
        , device : Device
        }
    | WaitingForGuest
        { host : String
        , name : String
        , device : Device
        }
      -- GUEST ONLY
    | JoiningHost
        { hostName : String
        , name : String
        , device : Device
        }
      -- BOTH
    | PlayersSet
        { player : Player
        , placedIslands : List ( Island, Coordinate )
        , unplacedIslands : List Island
        , selection : Selection
        , requestedPlacement : Maybe ( Island, Coordinate )
        , opponentReady : Bool
        , device : Device
        }
    | WaitingForOpponent
        { player : Player
        , placedIslands : List ( Island, Coordinate )
        , device : Device
        }
    | Playing PlayingData


type alias PlayingData =
    { player : Player
    , state : State
    , placedIslands : List ( Island, Coordinate )
    , opponentGuesses : List Coordinate
    , opponentTiles : List ( Coordinate, Bool )
    , forestedIslands : List Island
    , device : Device
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
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( GettingDevice flags
    , Task.perform GotViewport Browser.Dom.getViewport
    )



-- UPDATE


type Msg
    = GotViewport Browser.Dom.Viewport
    | ResizedViewport Int Int
    | UserChangedName String
    | UserPressedCreateGame
    | UserPressedJoinTheGame
    | ReceivedPlayerAdded ()
    | UserPressedTile Coordinate
    | UserPressedIslandTile Island Coordinate
    | ReceivedPositionedIsland { island : String, row : Int, col : Int }
    | ReceivedFailedPositioningIsland { island : String, row : Int, col : Int }
    | ReceivedOpponentSetIslands ()
    | UserPressedImReady
    | ReceivedBoard ()
    | UserPressedOpponentTile Coordinate
    | ReceivedFailedGuessingCoordinate ()
    | ReceivedPlayerGuessedCoordinate GuessedCoordinate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotViewport { viewport }, GettingDevice flags ) ->
            if flags.hash == "" then
                ( Lobby
                    { host = flags.protocol ++ "//" ++ flags.host
                    , name = ""
                    , device =
                        Element.classifyDevice
                            { width = Basics.round viewport.width
                            , height = Basics.round viewport.height
                            }
                    }
                , Cmd.none
                )

            else
                ( JoiningHost
                    { hostName = String.dropLeft 1 flags.hash
                    , name = ""
                    , device =
                        Element.classifyDevice
                            { width = Basics.round viewport.width
                            , height = Basics.round viewport.height
                            }
                    }
                , Cmd.none
                )

        ( ResizedViewport width height, GettingDevice flags ) ->
            if flags.hash == "" then
                ( Lobby
                    { host = flags.protocol ++ "//" ++ flags.host
                    , name = ""
                    , device =
                        Element.classifyDevice
                            { width = width
                            , height = height
                            }
                    }
                , Cmd.none
                )

            else
                ( JoiningHost
                    { hostName = String.dropLeft 1 flags.hash
                    , name = ""
                    , device =
                        Element.classifyDevice
                            { width = width
                            , height = height
                            }
                    }
                , Cmd.none
                )

        ( _, GettingDevice _ ) ->
            ( model, Cmd.none )

        -- ONLY HOST
        ( UserChangedName name, Lobby data ) ->
            ( Lobby { data | name = name }
            , Cmd.none
            )

        ( UserPressedCreateGame, Lobby { host, name, device } ) ->
            if name == "" then
                ( model, Cmd.none )

            else
                ( WaitingForGuest
                    { host = host
                    , name = name
                    , device = device
                    }
                , createGame name
                )

        ( ResizedViewport width height, Lobby data ) ->
            ( Lobby
                { data
                    | device =
                        Element.classifyDevice
                            { width = width
                            , height = height
                            }
                }
            , Cmd.none
            )

        ( _, Lobby _ ) ->
            ( model, Cmd.none )

        ( ReceivedPlayerAdded _, WaitingForGuest { device } ) ->
            ( PlayersSet
                { player = Host
                , placedIslands = []
                , unplacedIslands = allIslands
                , selection = None
                , requestedPlacement = Nothing
                , opponentReady = False
                , device = device
                }
            , Cmd.none
            )

        ( ResizedViewport width height, WaitingForGuest data ) ->
            ( WaitingForGuest
                { data
                    | device =
                        Element.classifyDevice
                            { width = width
                            , height = height
                            }
                }
            , Cmd.none
            )

        ( _, WaitingForGuest _ ) ->
            ( model, Cmd.none )

        -- ONLY GUEST
        ( UserChangedName name, JoiningHost data ) ->
            ( JoiningHost { data | name = name }
            , Cmd.none
            )

        ( UserPressedJoinTheGame, JoiningHost { hostName, name } ) ->
            ( model
            , joinGame
                { hostName = hostName
                , name = name
                }
            )

        ( ReceivedPlayerAdded _, JoiningHost { device } ) ->
            ( PlayersSet
                { player = Guest
                , placedIslands = []
                , unplacedIslands = allIslands
                , selection = None
                , requestedPlacement = Nothing
                , opponentReady = False
                , device = device
                }
            , Cmd.none
            )

        ( ResizedViewport width height, JoiningHost data ) ->
            ( JoiningHost
                { data
                    | device =
                        Element.classifyDevice
                            { width = width
                            , height = height
                            }
                }
            , Cmd.none
            )

        ( _, JoiningHost _ ) ->
            ( model, Cmd.none )

        -- BOTH
        ( UserPressedTile coordinate, PlayersSet data ) ->
            case data.selection of
                None ->
                    ( PlayersSet { data | selection = Tile coordinate }
                    , Cmd.none
                    )

                Tile _ ->
                    ( PlayersSet { data | selection = Tile coordinate }
                    , Cmd.none
                    )

                Island island offset ->
                    let
                        requestedCoordinate =
                            { row = coordinate.row - offset.row
                            , col = coordinate.col - offset.col
                            }
                    in
                    ( PlayersSet
                        { data
                            | selection = None
                            , requestedPlacement = Just ( island, requestedCoordinate )
                        }
                    , positionIsland
                        { island = islandToString island
                        , row = requestedCoordinate.row
                        , col = requestedCoordinate.col
                        }
                    )

        ( UserPressedIslandTile island offset, PlayersSet data ) ->
            case data.selection of
                None ->
                    ( PlayersSet { data | selection = Island island offset }
                    , Cmd.none
                    )

                Tile coordinate ->
                    let
                        requestedCoordinate =
                            { row = coordinate.row - offset.row
                            , col = coordinate.col - offset.col
                            }
                    in
                    ( PlayersSet
                        { data
                            | selection = None
                            , requestedPlacement = Just ( island, requestedCoordinate )
                        }
                    , positionIsland
                        { island = islandToString island
                        , row = requestedCoordinate.row
                        , col = requestedCoordinate.col
                        }
                    )

                Island _ _ ->
                    ( PlayersSet { data | selection = Island island offset }
                    , Cmd.none
                    )

        ( ReceivedPositionedIsland { island, row, col }, PlayersSet data ) ->
            case islandFromString island of
                Nothing ->
                    ( model, Cmd.none )

                Just island_ ->
                    case data.requestedPlacement of
                        Nothing ->
                            ( model, Cmd.none )

                        Just ( requestedIsland, coordinate ) ->
                            if island_ == requestedIsland && Coordinate row col == coordinate then
                                ( PlayersSet
                                    { data
                                        | selection = None
                                        , placedIslands =
                                            ( island_
                                            , { row = row
                                              , col = col
                                              }
                                            )
                                                :: data.placedIslands
                                        , unplacedIslands =
                                            List.filter
                                                (\unplacedIsland ->
                                                    unplacedIsland /= island_
                                                )
                                                data.unplacedIslands
                                        , requestedPlacement = Nothing
                                    }
                                , Cmd.none
                                )

                            else
                                ( model, Cmd.none )

        ( ReceivedFailedPositioningIsland { island, row, col }, PlayersSet data ) ->
            case islandFromString island of
                Nothing ->
                    ( model, Cmd.none )

                Just island_ ->
                    case data.requestedPlacement of
                        Nothing ->
                            ( model, Cmd.none )

                        Just ( requestedIsland, coordinate ) ->
                            if island_ == requestedIsland && Coordinate row col == coordinate then
                                ( PlayersSet
                                    { data
                                        | selection = None
                                        , requestedPlacement = Nothing
                                    }
                                , Cmd.none
                                )

                            else
                                ( model, Cmd.none )

        ( ReceivedOpponentSetIslands (), PlayersSet data ) ->
            ( PlayersSet { data | opponentReady = True }
            , Cmd.none
            )

        ( UserPressedImReady, PlayersSet data ) ->
            if data.opponentReady then
                ( Playing
                    { player = data.player
                    , state =
                        case data.player of
                            Host ->
                                Guessing

                            Guest ->
                                OpponentGuessing
                    , placedIslands = data.placedIslands
                    , opponentGuesses = []
                    , opponentTiles = []
                    , forestedIslands = []
                    , device = data.device
                    }
                , setIslands ()
                )

            else
                ( WaitingForOpponent
                    { player = data.player
                    , placedIslands = data.placedIslands
                    , device = data.device
                    }
                , setIslands ()
                )

        ( ResizedViewport width height, PlayersSet data ) ->
            ( PlayersSet
                { data
                    | device =
                        Element.classifyDevice
                            { width = width
                            , height = height
                            }
                }
            , Cmd.none
            )

        ( _, PlayersSet _ ) ->
            ( model, Cmd.none )

        ( ReceivedBoard (), WaitingForOpponent _ ) ->
            ( model, Cmd.none )

        ( ReceivedOpponentSetIslands (), WaitingForOpponent data ) ->
            ( Playing
                { player = data.player
                , state =
                    case data.player of
                        Host ->
                            Guessing

                        Guest ->
                            OpponentGuessing
                , placedIslands = data.placedIslands
                , opponentGuesses = []
                , opponentTiles = []
                , forestedIslands = []
                , device = data.device
                }
            , Cmd.none
            )

        ( ResizedViewport width height, WaitingForOpponent data ) ->
            ( WaitingForOpponent
                { data
                    | device =
                        Element.classifyDevice
                            { width = width
                            , height = height
                            }
                }
            , Cmd.none
            )

        ( _, WaitingForOpponent _ ) ->
            ( model, Cmd.none )

        -- PLAYING
        ( UserPressedOpponentTile coordinate, Playing data ) ->
            case data.state of
                Guessing ->
                    ( Playing { data | state = Guessed coordinate }
                    , guessCoordinate
                        { player = playerToString data.player
                        , row = coordinate.row
                        , col = coordinate.col
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        ( ReceivedFailedGuessingCoordinate (), Playing data ) ->
            ( model, Cmd.none )

        ( ReceivedPlayerGuessedCoordinate { player, row, col, result }, Playing data ) ->
            case ( data.state, playerFromString player ) of
                ( Guessed coordinate, Just guessingPlayer ) ->
                    if guessingPlayer == data.player && Coordinate row col == coordinate then
                        ( Playing
                            { data
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
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                ( OpponentGuessing, Just guessingPlayer ) ->
                    if guessingPlayer /= data.player then
                        ( Playing
                            { data
                                | state =
                                    if result.win == "win" then
                                        OpponentWon

                                    else
                                        Guessing
                                , opponentGuesses = Coordinate row col :: data.opponentGuesses
                            }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( ResizedViewport width height, Playing data ) ->
            ( Playing
                { data
                    | device =
                        Element.classifyDevice
                            { width = width
                            , height = height
                            }
                }
            , Cmd.none
            )

        ( _, Playing _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize ResizedViewport
        , case model of
            GettingDevice _ ->
                Sub.none

            Lobby _ ->
                Sub.none

            WaitingForGuest _ ->
                playerAdded ReceivedPlayerAdded

            JoiningHost _ ->
                playerAdded ReceivedPlayerAdded

            PlayersSet _ ->
                Sub.batch
                    [ positionedIsland ReceivedPositionedIsland
                    , failedPositioningIsland ReceivedFailedPositioningIsland
                    , opponentSetIslands ReceivedOpponentSetIslands
                    , receivedBoard ReceivedBoard
                    ]

            WaitingForOpponent _ ->
                opponentSetIslands ReceivedOpponentSetIslands

            Playing data ->
                case data.state of
                    Guessed _ ->
                        Sub.batch
                            [ failedGuessingCoordinate ReceivedFailedGuessingCoordinate
                            , playerGuessedCoordinate ReceivedPlayerGuessedCoordinate
                            ]

                    OpponentGuessing ->
                        Sub.batch
                            [ failedGuessingCoordinate ReceivedFailedGuessingCoordinate
                            , playerGuessedCoordinate ReceivedPlayerGuessedCoordinate
                            ]

                    _ ->
                        Sub.none
        ]



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
            , normal
            ]
            (case model of
                GettingDevice _ ->
                    Element.none

                Lobby { name } ->
                    viewLobby name

                WaitingForGuest { host, name } ->
                    viewWaitingForGuest host name

                JoiningHost { hostName, name } ->
                    viewJoiningHost hostName name

                PlayersSet { placedIslands, unplacedIslands, selection, opponentReady, device } ->
                    viewPlayersSet device placedIslands unplacedIslands selection opponentReady

                WaitingForOpponent { placedIslands, device } ->
                    viewWaitingForOpponent device placedIslands

                Playing data ->
                    viewPlaying data
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
        [ Element.el
            [ Region.heading 1
            , Element.centerX
            , large
            ]
            (Element.text "Welcome to Islands!")
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level2
            , Element.width Element.fill
            ]
            [ Input.text
                [ Input.focusedOnLoad
                , Element.htmlAttribute <|
                    Html.Events.on "keypress"
                        (Decode.field "key" Decode.string
                            |> Decode.andThen
                                (\rawKey ->
                                    case rawKey of
                                        "Enter" ->
                                            Decode.succeed UserPressedCreateGame

                                        _ ->
                                            Decode.fail "not handling that key here"
                                )
                        )
                ]
                { onChange = UserChangedName
                , text = name
                , placeholder = Nothing
                , label =
                    Input.labelAbove []
                        (Element.text "Pick a name")
                }
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
        [ Element.el
            [ Region.heading 1
            , Element.centerX
            , large
            ]
            (Element.text ("Hi " ++ name ++ "!"))
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
        [ Element.el
            [ Region.heading 1
            , Element.centerX
            , large
            ]
            (Element.text "Welcome to Island!")
        , Element.el
            [ Element.centerX ]
            (Element.text (hostName ++ " is inviting you to play."))
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level2
            , Element.width Element.fill
            ]
            [ Input.text
                [ Input.focusedOnLoad
                , Element.htmlAttribute <|
                    Html.Events.on "keypress"
                        (Decode.field "key" Decode.string
                            |> Decode.andThen
                                (\rawKey ->
                                    case rawKey of
                                        "Enter" ->
                                            Decode.succeed UserPressedJoinTheGame

                                        _ ->
                                            Decode.fail "not handling that key here"
                                )
                        )
                ]
                { onChange = UserChangedName
                , text = name
                , placeholder = Nothing
                , label =
                    Input.labelAbove []
                        (Element.text "Pick a name")
                }
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
        [ Element.el
            [ Region.heading 1
            , Element.centerX
            , large
            ]
            (Element.text "Place your Islands!")
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level3 ]
            [ Element.column
                [ Element.spacing Ui.Theme.Spacing.level1
                , Element.centerX
                ]
                (List.map (viewTileRow device placedIslands selection) (List.range minRange maxRange))
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


viewTileRow : Device -> List ( Island, Coordinate ) -> Selection -> Int -> Element Msg
viewTileRow device placedIslands selection row =
    Element.row
        [ Element.spacing Ui.Theme.Spacing.level1 ]
        (List.map (viewTile device placedIslands selection row) (List.range minRange maxRange))


viewTile : Device -> List ( Island, Coordinate ) -> Selection -> Int -> Int -> Element Msg
viewTile device placedIslands selection row col =
    let
        selected =
            case selection of
                Tile coordinate ->
                    coordinate == Coordinate row col

                _ ->
                    False

        isIsland =
            List.any
                (\( island, islandCoordinate ) ->
                    Set.member
                        ( row, col )
                        (offsets island
                            |> List.map
                                (\coordinate ->
                                    ( islandCoordinate.row + coordinate.row
                                    , islandCoordinate.col + coordinate.col
                                    )
                                )
                            |> Set.fromList
                        )
                )
                placedIslands

        role =
            if isIsland then
                Tile.Island

            else
                Tile.Water
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


viewWaitingForOpponent : Device -> List ( Island, Coordinate ) -> Element Msg
viewWaitingForOpponent device placedIslands =
    Element.column
        [ Element.centerX
        , Element.paddingXY 0 Ui.Theme.Spacing.level3
        , Element.spacing Ui.Theme.Spacing.level3
        , Region.mainContent
        ]
        [ Element.el
            [ Region.heading 1
            , Element.centerX
            , large
            ]
            (Element.text "Place your Islands!")
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level3 ]
            [ Element.column
                [ Element.spacing Ui.Theme.Spacing.level1
                , Element.centerX
                ]
                (List.map (viewTileRow device placedIslands None) (List.range minRange maxRange))
            , Element.el
                [ Element.centerX ]
                (Element.text "Waiting for other player.")
            ]
        ]


viewPlaying : PlayingData -> Element Msg
viewPlaying data =
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
                    [ Element.el
                        [ Region.heading 1
                        , Element.centerX
                        , large
                        ]
                        (Element.text "Opponent's sea")
                    , Element.column
                        [ Element.spacing Ui.Theme.Spacing.level1
                        , Element.centerX
                        ]
                        (List.map (viewOpponentTileRow data.device data.opponentTiles)
                            (List.range minRange maxRange)
                        )
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
                [ Element.el
                    [ Region.heading 1
                    , Element.centerX
                    , large
                    ]
                    (Element.text "Your sea")
                , Element.column
                    [ Element.spacing Ui.Theme.Spacing.level1
                    , Element.centerX
                    ]
                    (List.map (viewYourTileRow data.device data.placedIslands data.opponentGuesses)
                        (List.range minRange maxRange)
                    )
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


viewOpponentTileRow : Device -> List ( Coordinate, Bool ) -> Int -> Element Msg
viewOpponentTileRow device opponentTiles row =
    Element.row
        [ Element.spacing Ui.Theme.Spacing.level1 ]
        (List.map (viewOpponentTile device opponentTiles row) (List.range minRange maxRange))


viewOpponentTile : Device -> List ( Coordinate, Bool ) -> Int -> Int -> Element Msg
viewOpponentTile device opponentTiles row col =
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


viewYourTileRow : Device -> List ( Island, Coordinate ) -> List Coordinate -> Int -> Element Msg
viewYourTileRow device placedIslands opponentGuesses row =
    Element.row
        [ Element.spacing Ui.Theme.Spacing.level1 ]
        (List.map (viewYourTile device placedIslands opponentGuesses row)
            (List.range minRange maxRange)
        )


viewYourTile : Device -> List ( Island, Coordinate ) -> List Coordinate -> Int -> Int -> Element Msg
viewYourTile device placedIslands opponentGuesses row col =
    let
        isIsland =
            List.any
                (\( island, islandCoordinate ) ->
                    Set.member
                        ( row, col )
                        (offsets island
                            |> List.map
                                (\coordinate ->
                                    ( islandCoordinate.row + coordinate.row
                                    , islandCoordinate.col + coordinate.col
                                    )
                                )
                            |> Set.fromList
                        )
                )
                placedIslands

        isGuessed =
            List.any (\coordinate -> coordinate == Coordinate row col) opponentGuesses

        role =
            if isIsland then
                if isGuessed then
                    Tile.Forest

                else
                    Tile.Island

            else if isGuessed then
                Tile.DiscoveredWater

            else
                Tile.Water
    in
    tile device
        |> Tile.withRole role
        |> Tile.toElement



-- FONT SIZES


large : Element.Attribute msg
large =
    Font.size 36


normal : Element.Attribute msg
normal =
    Font.size 24
