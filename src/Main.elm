module Main exposing (..)

{-| This is a skeleton for an interpreter application. For now it basically simply display what you type in.
You should just:
  - insert your code in the update and the viewShapes functions,
  - surely add some field in the Model type.
You can of course add other types,
functions and modules but you shouldn't have to modify the code at other places -- if you think you have to modify
this code, reach your teacher out before doing this.
-}

import Browser
import Browser.Dom
import Collage exposing (Collage)
import Collage.Render
import Collage.Text
import Color
import Element exposing (Element, centerX, centerY, column, el, fill, focusStyle, height, padding, paddingEach, px, rgb, rgba, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Task
import Collage.Text exposing (Line(..))
import Parser exposing (..)
import Debug

type alias Model =
    { commandInput : String
    , history : List String
    }

type Msg
    = CommandEntered String
    | CommandSubmitted 
    | NoOp    

-- Allow only one char
oneCharParsed : Parser String
oneCharParsed =
    chompIf Char.isAlpha
        |> getChompedString

type alias Point =
    { name: String
    , x : Float
    , y : Float
    }

pointParser : Parser Point
pointParser =
  succeed Point
    |= oneCharParsed
    |. symbol "="
    |. symbol "("
    |= float
    |. symbol ","
    |= float
    |. symbol ")"

init : Model
init =
    { commandInput = ""
    , history = []
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        CommandEntered command ->
            { model | commandInput = command }

        CommandSubmitted ->
            -- TODO: Command parsing and applying goes here!!!
            { model | commandInput = "" , history = model.history ++ [ model.commandInput ] }

        NoOp ->
            model


view : Model -> Element Msg
view model =
    column [ width fill, height fill ]
        [ el [ width fill, height fill ]
            (el [ Border.width 2, padding 2, centerX, centerY ]
                (Element.html (Collage.Render.svgBox ( 800, 800 ) (viewShapes model)))
            )
        , row [ width fill, height (px 50) ]
            [ el [] (text "Your command: ")
            , Input.text
                [ onEnter CommandSubmitted
                , Element.htmlAttribute (Html.Attributes.id "prompt")
                , width fill
                ]
                { onChange = CommandEntered
                , text = model.commandInput
                , placeholder = Nothing
                , label = Input.labelHidden "Enter the command"
                }
            ]
        ]


viewShapes : Model -> Collage Msg
viewShapes model =
    -- TODO: change this function! Here are some examples to draw basic shapes.
    case run pointParser model.commandInput of 
        Ok point ->
            Collage.group
                [
                    Collage.circle 3
                        |> Collage.filled (Collage.uniform Color.black)
                        |> Collage.shift ( point.x, point.y )
                    , Collage.Text.fromString point.name
                        |> Collage.rendered
                        |> Collage.shift ( point.x + 15, point.y + 3 )
                ] 
        Err err -> Collage.group [ ] 
    
onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )

main =
    Browser.element
        { init =
            \() ->
                ( init
                , Browser.Dom.focus "prompt"
                    |> Task.attempt (always NoOp)
                )
        , view =
            \model ->
                Element.layout
                    [ width fill
                    , height fill
                    ]
                    (view model)
        , update =
            \msg model ->
                ( update msg model
                , if msg == CommandSubmitted then
                    Browser.Dom.focus "prompt"
                        |> Task.attempt (always NoOp)

                  else
                    Cmd.none
                )
        , subscriptions = always Sub.none
        }