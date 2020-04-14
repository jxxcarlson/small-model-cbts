module Main exposing (..)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import List.Extra
import OrderSequence exposing (Future)
import SimpleGraph exposing (Option(..))
import State exposing (State)
import Style
import Time
import Widget.Button as Button exposing (Size(..))
import Widget.Style as WS
import Widget.TextField as TextField
import World exposing (World)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { world : World
    , history : List State
    , runState : RunState
    , counter : Int
    }


type RunState
    = Running
    | BatchDone
    | Paused
    | End


type RunMode
    = Single
    | Batch


type BatchJobState
    = NoBatch
    | EndBatch
    | InTrial Int



-- MSG


type Msg
    = NoOp
    | Tick Time.Posix
    | Step
    | Reset


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    { world = World.init State.initialState OrderSequence.orderSequence1
    , history = [ State.initialState ]
    , runState = Paused
    , counter = 0
    }
        |> withNoCmd


subscriptions model =
    Time.every 4000 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick _ ->
            model |> withNoCmd

        Step ->
            let
                newWorld =
                    World.update model.world
            in
            { model
                | world = newWorld
                , history = newWorld.state :: model.history
            }
                |> withNoCmd

        Reset ->
            model |> withNoCmd



-- HELPER
--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle WS.noFocus ] } [ centerX ] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column Style.mainColumn
        [ el [ centerX, Font.bold, Font.color Style.lightColor, paddingEach { emptyPadding | top = 30 } ] (text "State")
        , column [ centerX, spacing 5, padding 20, Background.color Style.lightColor, width (px 1000) ]
            (viewHistory model.history
                ++ [ stepButton ]
            )
        , footer model
        ]


emptyPadding =
    { left = 0, right = 0, top = 0, bottom = 0 }


stringFormatter s =
    el [ Font.size 12, alignRight, width (px 20) ] (text s)


viewState : State -> Element Msg
viewState state =
    column [ width (px 20) ] (List.map stringFormatter (State.stringVal state))


labels4 : Element msg
labels4 =
    column [ spacing 4, width (px 20) ] (List.map stringFormatter State.labels)


labels =
    List.map stringFormatter State.labels


bg : Int -> Attr decorative msg
bg k =
    case modBy 2 k == 0 of
        True ->
            Background.color Style.paleBlue

        False ->
            Background.color Style.whiteColor


viewHistory : List State -> List (Element msg)
viewHistory states =
    List.map State.stringVal states
        |> (\x -> x ++ [ State.labels ])
        |> List.Extra.transpose
        |> List.map List.reverse
        |> List.map (List.map stringFormatter)
        |> List.indexedMap (\k r -> row [ bg k, spacing 4, padding 4 ] r)


stepButton =
    Button.make Step "Step"
        |> Button.withWidth (Bounded 100)
        |> Button.withSelected False
        |> Button.toElement


footer model =
    row
        [ alignBottom
        , paddingXY 10 0
        , Font.size 14
        , spacing 15
        , centerX
        , Background.color Style.lightColor
        , width fill
        , height (px 40)
        ]
        []
