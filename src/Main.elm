module Main exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Types exposing (Accent(..), Color(..), Hue(..), Opacity(..), Target(..))


main : Html msg
main =
    Html.div [ colorAttrs.font.op100.normal.primary ]
        [ Html.text "Hello"
        ]


colorAttrs : Types.AllColors (Attribute msg)
colorAttrs =
    Types.allColors colorToAttr


colorToAttr : Color -> Attribute msg
colorToAttr (Color target opacity accent hue) =
    let
        targetFragment : String
        targetFragment =
            case target of
                Background ->
                    "background-color"

                Font ->
                    "color"

        opacityFragment : String
        opacityFragment =
            case opacity of
                Op100 ->
                    String.fromFloat 1.0

                Op70 ->
                    String.fromFloat 0.7

                Op30 ->
                    String.fromFloat 0.3

        rgbFragment : String
        rgbFragment =
            case ( accent, hue ) of
                ( Normal, Primary ) ->
                    "13, 110, 253"

                ( Normal, Secondary ) ->
                    "108, 117, 125"

                ( Normal, Danger ) ->
                    "220, 53, 69"

                ( Subtle, Primary ) ->
                    "207, 226, 255"

                ( Subtle, Secondary ) ->
                    "226, 227, 229"

                ( Subtle, Danger ) ->
                    "241, 174, 181"
    in
    Attr.style targetFragment <|
        String.concat [ "rgba(", rgbFragment, ", ", opacityFragment, ")" ]
