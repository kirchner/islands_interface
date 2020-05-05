module Ui.Atom.InputText exposing
    ( InputText, inputText
    , toElement
    , focusedOnLoad
    )

{-|

@docs InputText, inputText
@docs toElement
@docs focusedOnLoad

-}

import Element exposing (Element)
import Element.Input as Input
import Html.Events
import Json.Decode as Decode


{-| -}
type InputText msg
    = InputText Options (String -> msg) msg String String


{-| -}
inputText : (String -> msg) -> msg -> String -> String -> InputText msg
inputText onChange onReturn label content =
    InputText defaultOptions onChange onReturn label content


type alias Options =
    { focusedOnLoad : Bool }


defaultOptions : Options
defaultOptions =
    { focusedOnLoad = False }


focusedOnLoad : InputText msg -> InputText msg
focusedOnLoad (InputText options onChange onReturn label content) =
    InputText { options | focusedOnLoad = True } onChange onReturn label content


toElement : InputText msg -> Element msg
toElement (InputText options onChange onReturn label content) =
    let
        addFocusedOnLoad attributes =
            if options.focusedOnLoad then
                Input.focusedOnLoad :: attributes

            else
                attributes
    in
    Input.text
        ([ Element.htmlAttribute <|
            Html.Events.on "keypress"
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\rawKey ->
                            case rawKey of
                                "Enter" ->
                                    Decode.succeed onReturn

                                _ ->
                                    Decode.fail "not handling that key here"
                        )
                )
         ]
            |> addFocusedOnLoad
        )
        { onChange = onChange
        , text = content
        , placeholder = Nothing
        , label =
            Input.labelAbove []
                (Element.text label)
        }
