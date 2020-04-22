module Main exposing (..)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Config exposing (Config, LabeledItem, configurations)
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
import Unit.Time as UT
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
    , config : Config
    , configurationIndex : Int
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
    | CycleConfig
    | Run


type alias Flags =
    {}


getConfig : Int -> Config
getConfig k =
    List.Extra.getAt k configurations |> Maybe.withDefault Config.default |> Debug.log "CONFIG"


initialModel k =
    let
        initialState =
            State.initialStateWithConfig <| getConfig k
    in
    { world = World.init initialState Future.futureV1
    , history = [ initialState ]
    , config = getConfig k
    , configurationIndex = k
    , runState = Paused
    , counter = 0
    }


bareInit : Int -> ( Model, Cmd Msg )
bareInit k =
    initialModel k |> withNoCmd


init : Flags -> ( Model, Cmd Msg )
init flags =
    bareInit 0


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
                    updateWorld model.config model |> withNoCmd

                False ->
                    model |> withNoCmd

        Run ->
            let
                newModel =
                    runWorld model.config model
            in
            newModel |> withNoCmd

        Reset ->
            initialModel model.configurationIndex |> withNoCmd

        CycleConfig ->
            let
                k =
                    if model.configurationIndex + 1 >= List.length configurations then
                        0

                    else
                        model.configurationIndex + 1
            in
            initialModel k |> withNoCmd


updateWorld : Config -> Model -> Model
updateWorld config model =
    let
        newWorld =
            World.update config model.world
    in
    { model
        | world = newWorld
        , history = newWorld.state :: model.history
    }


runWorld : Config -> Model -> Model
runWorld config model =
    let
        n =
            Future.length model.world.future - 1
    in
    List.foldl (\n_ model_ -> updateWorld config model_) model (List.range 0 n)



-- VIEW


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle WS.noFocus ] } [ centerX ] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column Style.mainColumn
        [ el [ centerX, Font.bold, Font.color Style.lightColor, paddingEach { emptyPadding | top = 30 } ] (text "Simple Simulator")
        , row [ spacing 12 ] [ viewHistoryAndConfiguration model, viewLog model ]
        , footer model
        ]


viewHistoryAndConfiguration model =
    column [ spacing 8, alignTop ]
        [ viewHistory_ model
        , viewConfiguration model
        ]


viewConfiguration : Model -> Element Msg
viewConfiguration model =
    let
        viewConfigItem : LabeledItem -> Element Msg
        viewConfigItem data =
            row [ spacing 12, Font.size 12 ]
                [ el [ width (px 100), Font.bold ] (text data.label)
                , el [ width (px 250) ] (text data.value)
                ]
    in
    column [ paddingXY 12 8, spacing 8, width (px 800), alignTop, Background.color Style.lightColor ] (List.map viewConfigItem (Config.stringValue model.config))


viewHistory_ model =
    column [ centerX, alignTop, spacing 5, padding 20, Background.color Style.charcoal, width (px 800) ]
        (viewHistory model.history
            ++ [ row [ paddingEach { emptyPadding | top = 10 }, spacing 12 ] [ cycleConfigsButton model, resetButton, stepButton, runButton ] ]
            ++ [ row [ paddingEach { emptyPadding | top = 20 } ] [ legend ] ]
        )


viewLog : Model -> Element Msg
viewLog model =
    column
        [ spacing 6
        , width (px 400)
        , height (px 650)
        , Background.color Style.whiteColor
        , alignTop
        , scrollbarY
        , paddingXY 12 12
        ]
        (List.map viewMessage (List.reverse <| State.log model.world.state))


viewMessage : ( UT.Time, String, String ) -> Element Msg
viewMessage ( t, m, note ) =
    row [ Font.size 12 ]
        [ el [ width (px 25), alignRight ] (text (UT.stringVal <| UT.increment t))
        , el [ width (px 150) ] (text m)
        , el [ width (px 100) ] (text note)
        ]


legend =
    column [ spacing 8 ]
        [ row [] [ legend1 ]
        , row [] [ legend2 ]
        ]


legend1 =
    row [ spacing 8, Font.color Style.lightColor ]
        (List.map legendFormatter legendItems1)


legend2 =
    row [ spacing 8, Font.color Style.lightColor ]
        (List.map legendFormatter legendItems2)


legendItems1 =
    [ "T: time"
    , "|"
    , "FI: Fiat balance"
    , "|"
    , "CC: CC balance"
    , "|"
    , "ST: Stock"
    , "|"
    , "OF: Orders filled"
    , "|"
    , "OL: Orders lost"
    , "|"
    , "OO: Original order"
    ]


legendItems2 =
    [ "CO: customer order"
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


labels =
    List.map stringFormatter State.labels


bg : Int -> Attr decorative msg
bg k =
    if k == 3 || k == 6 then
        Background.color Style.rowX

    else if k == 5 then
        Background.color Style.rowY

    else
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


runButton =
    Button.make Run "Run"
        |> Button.withWidth (Bounded 100)
        |> Button.withSelected False
        |> Button.toElement


resetButton =
    Button.make Reset "Reset"
        |> Button.withWidth (Bounded 100)
        |> Button.withSelected False
        |> Button.toElement


cycleConfigsButton model =
    Button.make CycleConfig ("Cycle Config (" ++ String.fromInt model.configurationIndex ++ ")")
        |> Button.withWidth (Bounded 120)
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
