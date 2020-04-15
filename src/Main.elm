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
import Future exposing (Future)
import Html exposing (Html)
import List.Extra
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


initialModel =
    { world = World.init State.initialState Future.futureV1
    , history = [ State.initialState ]
    , runState = Paused
    , counter = 0
    }


bareInit : ( Model, Cmd Msg )
bareInit =
    initialModel |> withNoCmd


init : Flags -> ( Model, Cmd Msg )
init flags =
    bareInit


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
            case Future.length model.world.future > 0 of
                True ->
                    updateWorld model |> withNoCmd

                False ->
                    model |> withNoCmd

        Reset ->
            initialModel |> withNoCmd


updateWorld : Model -> Model
updateWorld model =
    let
        newWorld =
            World.update model.world
    in
    { model
        | world = newWorld
        , history = newWorld.state :: model.history
    }



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
        , column [ centerX, spacing 5, padding 20, Background.color Style.charcoal, width (px 1020) ]
            (viewHistory model.history
                ++ [ row [ paddingEach { emptyPadding | top = 10 }, spacing 12 ] [ resetButton, stepButton ] ]
                ++ [ row [ paddingEach { emptyPadding | top = 20 } ] [ legend ] ]
            )
        , footer model
        ]


legend =
    row [ spacing 8, Font.color Style.lightColor ]
        (List.map legendFormatter legendItems)


legendItems =
    [ "t: time"
    , "|"
    , "fi: Fiat balance"
    , "|"
    , "cc: CC balance"
    , "|"
    , "st: Stock"
    , "|"
    , "of: Orders filled"
    , "|"
    , "ol: Orders lost"
    , "|"
    , "OO: Original order"
    , "|"
    , "CO: customer order"
    , "|"
    , "BO: business order"
    ]


legendFormatter str =
    el [ Font.size 14 ] (text str)


emptyPadding =
    { left = 0, right = 0, top = 0, bottom = 0 }


stringFormatter s =
    el [ Font.size 12, width (px 20), paddingEach { emptyPadding | right = 4 } ] (el [ alignRight ] (text s))


viewState : State -> Element Msg
viewState state =
    column [ width (px 20) ] (List.map stringFormatter (State.stringVal state))



--
--labels4 : Element msg
--labels4 =
--    column [ spacing 4, width (px 20) ] (List.map stringFormatter State.labels)


labels =
    List.map stringFormatter State.labels


bg : Int -> Attr decorative msg
bg k =
    case modBy 2 k == 0 of
        True ->
            Background.color Style.rowA

        False ->
            Background.color Style.rowB


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


resetButton =
    Button.make Reset "Reset"
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
