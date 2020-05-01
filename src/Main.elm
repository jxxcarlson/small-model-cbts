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
import Generate
import Html exposing (Html)
import List.Extra
import SimpleGraph exposing (Option(..))
import Stat
import State exposing (State)
import Style
import Time
import Unit.Money as Money exposing (Money)
import Unit.Time as UT
import Unit.Unit as Unit exposing (Unit)
import Widget.Button as Button exposing (Size(..))
import Widget.Style as WS
import Widget.TextField as TextField exposing (LabelPosition(..))
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
    , demandMean : String
    , demandSpread : String
    , initialSeedAsString : String
    , cycleLengthAsString : String
    , ccEarningsAsString : String
    , ccRatioAsString : String
    , initialFiatBalanceAsString : String
    , initialStockAsString : String
    , lowOrderAsString : String
    , highOrderAsString : String
    , thresholdAsString : String
    }


parameters : Model -> Parameters
parameters m =
    { fiatBal_ = m.initialFiatBalanceAsString
    , initialStock_ = m.initialStockAsString
    , ccEarnings_ = m.ccEarningsAsString
    , ccRatio_ = m.ccRatioAsString
    , cycleLength_ = m.cycleLengthAsString
    , seed_ = m.initialSeedAsString
    , lowOrder_ = m.lowOrderAsString
    , highOrder_ = m.highOrderAsString
    , threshold_ = m.thresholdAsString
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
    | EnterDemandMean String
    | EnterDemandSpread String
    | EnterSeed String
    | EnterCycleLength String
    | EnterCCEarnings String
    | EnterCCRatio String
    | EnterInitialFiatBalance String
    | EnterInitialStock String
    | EnterLowOrder String
    | EnterHighOrder String
    | EnterThreshold String


type alias Flags =
    {}


getConfig : Int -> Config
getConfig k =
    List.Extra.getAt k configurations |> Maybe.withDefault Config.default |> Debug.log "CONFIG"


type alias Parameters =
    { fiatBal_ : String
    , initialStock_ : String
    , ccEarnings_ : String
    , ccRatio_ : String
    , cycleLength_ : String
    , seed_ : String
    , lowOrder_ : String
    , highOrder_ : String
    , threshold_ : String
    }


configWithParameters : Parameters -> Config -> Config
configWithParameters p c =
    { c
        | initialStock = Unit.create (String.toInt p.initialStock_ |> Maybe.withDefault 0)
        , initialFiatBalance = Money.create (String.toFloat p.fiatBal_ |> Maybe.withDefault 0)
        , initialCCBalance = Money.create (String.toFloat p.ccEarnings_ |> Maybe.withDefault 0)
        , ccRatio = String.toFloat p.ccRatio_ |> Maybe.withDefault 0
        , lowOrder = Unit.create (String.toInt p.lowOrder_ |> Maybe.withDefault 0)
        , highOrder = Unit.create (String.toInt p.highOrder_ |> Maybe.withDefault 0)
        , stockOnHandThreshold = Unit.create (String.toInt p.threshold_ |> Maybe.withDefault 0)
    }


initialParameters =
    { fiatBal_ = "0"
    , initialStock_ = "0"
    , ccEarnings_ = "0"
    , ccRatio_ = "0"
    , cycleLength_ = "30"
    , seed_ = "1234"
    , lowOrder_ = "5"
    , highOrder_ = "20"
    , threshold_ = "10"
    }


initialModel : Parameters -> Int -> Int -> Int -> Model
initialModel p configIndex demandMean demandSpread =
    let
        baseConfig =
            getConfig configIndex

        newConfig =
            Debug.log "NEW CONFIG" <|
                configWithParameters p baseConfig

        initialState =
            Debug.log "INIT STATE" <|
                State.initialStateWithConfig newConfig

        seed =
            String.toInt p.seed_ |> Maybe.withDefault 1234

        demandRunLength =
            String.toInt p.cycleLength_ |> Maybe.withDefault 30

        future =
            Future.generateListWithMean seed demandMean demandSpread demandRunLength
    in
    { world = World.init initialState future
    , history = [ initialState ]
    , config = newConfig
    , configurationIndex = configIndex
    , runState = Paused
    , counter = 0
    , demandMean = String.fromInt demandMean
    , demandSpread = String.fromInt demandSpread
    , initialSeedAsString = p.seed_
    , cycleLengthAsString = p.cycleLength_
    , ccEarningsAsString = p.ccEarnings_
    , ccRatioAsString = p.ccRatio_
    , initialFiatBalanceAsString = p.fiatBal_
    , initialStockAsString = p.initialStock_
    , lowOrderAsString = p.lowOrder_
    , highOrderAsString = p.highOrder_
    , thresholdAsString = p.threshold_
    }


bareInit : Int -> ( Model, Cmd Msg )
bareInit k =
    initialModel initialParameters k 10 5 |> withNoCmd


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
                m =
                    model.demandMean |> String.toInt |> Maybe.withDefault 10

                s =
                    model.demandSpread |> String.toInt |> Maybe.withDefault 5

                model_ =
                    initialModel (parameters model) model.configurationIndex m s

                newModel =
                    runWorld model_.config model_
            in
            newModel |> withNoCmd

        Reset ->
            let
                m =
                    model.demandMean |> String.toInt |> Maybe.withDefault 10

                s =
                    model.demandSpread |> String.toInt |> Maybe.withDefault 5
            in
            initialModel (parameters model) model.configurationIndex m s |> withNoCmd

        CycleConfig ->
            let
                k =
                    if model.configurationIndex + 1 >= List.length configurations then
                        0

                    else
                        model.configurationIndex + 1

                m =
                    model.demandMean |> String.toInt |> Maybe.withDefault 10

                s =
                    model.demandSpread |> String.toInt |> Maybe.withDefault 5
            in
            initialModel (parameters model) k m s |> withNoCmd

        EnterDemandMean str ->
            { model | demandMean = str } |> withNoCmd

        EnterDemandSpread str ->
            { model | demandSpread = str } |> withNoCmd

        EnterSeed str ->
            { model | initialSeedAsString = str } |> withNoCmd

        EnterCycleLength str ->
            { model | cycleLengthAsString = str } |> withNoCmd

        EnterCCEarnings str ->
            { model | ccEarningsAsString = str } |> withNoCmd

        EnterCCRatio str ->
            { model | ccRatioAsString = str } |> withNoCmd

        EnterInitialFiatBalance str ->
            { model | initialFiatBalanceAsString = str } |> withNoCmd

        EnterInitialStock str ->
            { model | initialStockAsString = str } |> withNoCmd

        EnterLowOrder str ->
            { model | lowOrderAsString = str } |> withNoCmd

        EnterHighOrder str ->
            { model | highOrderAsString = str } |> withNoCmd

        EnterThreshold str ->
            { model | thresholdAsString = str } |> withNoCmd


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
        [ el [ centerX, Font.bold, Font.color Style.lightColor, paddingEach { emptyPadding | top = 15 } ] (text "Simple Simulator")
        , row [ spacing 12 ] [ viewHistoryAndConfiguration model, viewLog model ]

        -- , footer model
        ]


viewHistoryAndConfiguration model =
    column [ spacing 8, alignTop, Background.color Style.lightColor ]
        [ viewHistory_ model
        , viewFirstAndLastState model
        , viewResults model
        ]


viewResults : Model -> Element Msg
viewResults model =
    column [ Font.size 12, paddingXY 12 8, spacing 8, width (px 800), alignTop, Background.color Style.lightColor ]
        [ viewUnit "Business Orders" (State.sumRowOfUnits State.getBusinessOrder model.history)
        , viewUnit "Orders Lost" (State.getOrdersLost (State.last model.history))
        , viewStats "Demand"
            (State.sumRowOfUnits State.getDemand model.history)
            (meanDemand model |> roundTo 1)
            (stdevDemand model |> roundTo 1)
        , viewStats "Supply"
            (supply model)
            (meanSupply model |> roundTo 1)
            (stdevSupply model |> roundTo 1)
        , viewExcess model
        ]


viewUnit : String -> Unit -> Element Msg
viewUnit str u =
    row [ spacing 8 ]
        [ el [ width (px 100), Font.bold ] (text str)
        , el [ width (px 100) ] (text <| String.fromInt <| Unit.value u)
        ]


viewMoney : String -> Money -> Element Msg
viewMoney str u =
    row [ spacing 8 ]
        [ el [ width (px 100), Font.bold ] (text str)
        , el [ width (px 100) ] (text <| String.fromFloat <| Money.value u)
        ]


viewFloat : String -> Float -> Element Msg
viewFloat str u =
    row [ spacing 8 ]
        [ el [ width (px 100), Font.bold ] (text str)
        , el [ width (px 100) ] (text <| String.fromFloat u)
        ]


viewText : String -> String -> Element Msg
viewText str u =
    row [ spacing 8 ]
        [ el [ width (px 100), Font.bold ] (text str)
        , el [ width (px 100) ] (text u)
        ]


viewStats : String -> Unit -> Float -> Float -> Element Msg
viewStats str u v w =
    row [ spacing 8 ]
        [ el [ width (px 100), Font.bold ] (text str)
        , el [ width (px 30) ] (text <| String.fromInt <| Unit.value u)
        , el [ width (px 30) ] (text <| String.fromFloat v)
        , el [ width (px 30) ] (text <| String.fromFloat w)
        ]


viewExcess model =
    let
        d =
            demand model

        s =
            supply model
    in
    if Unit.gt d s then
        viewUnit "Excess Demand" (Unit.sub (demand model) (supply model))

    else if Unit.lt d s then
        viewUnit "Excess Supply" (Unit.sub (supply model) (demand model))

    else
        viewText "Supply-Demand" "Equilibrium"


demand : Model -> Unit
demand model =
    State.sumRowOfUnits State.getDemand model.history


meanDemand : Model -> Float
meanDemand model =
    let
        xs =
            List.map (Unit.value << State.getDemand) model.history
    in
    Generate.mean xs


stdevDemand : Model -> Float
stdevDemand model =
    let
        xs =
            List.map (toFloat << Unit.value << State.getDemand) model.history
    in
    Stat.stdev identity xs |> Maybe.withDefault 0


roundTo : Int -> Float -> Float
roundTo k x =
    let
        factor =
            10 ^ toFloat k
    in
    toFloat (round (factor * x)) / factor


meanSupply : Model -> Float
meanSupply model =
    State.initialStock model.history
        :: List.map State.getBusinessOrder model.history
        |> List.map Unit.value
        |> Generate.mean


stdevSupply : Model -> Float
stdevSupply model =
    let
        xs =
            List.map (toFloat << Unit.value << State.getBusinessOrder) model.history
    in
    Stat.stdev identity xs |> Maybe.withDefault 0


supply : Model -> Unit
supply model =
    let
        ordersPlaced =
            State.sumRowOfUnits State.getBusinessOrder model.history

        initialStock_ =
            State.initialStock model.history
    in
    Unit.add initialStock_ ordersPlaced


firstState : Model -> Maybe State
firstState m =
    m.history
        |> List.reverse
        |> List.head


lastState : Model -> Maybe State
lastState m =
    m.history
        |> List.head


viewFirstAndLastState : Model -> Element Msg
viewFirstAndLastState m =
    row [ spacing 24, Font.size 12, paddingXY 12 12 ]
        [ viewFirstState m
        , viewLastState m
        ]


viewFirstState : Model -> Element Msg
viewFirstState m =
    case firstState m of
        Nothing ->
            Element.none

        Just s ->
            column [ spacing 8 ]
                [ viewUnit "Initial Stock" (State.stockOnHand s)
                , viewMoney "Fiat Balance" (State.fiatBalance s)
                , viewMoney "CC Balance" (State.ccBalance s)
                ]


viewLastState : Model -> Element Msg
viewLastState m =
    case lastState m of
        Nothing ->
            Element.none

        Just s ->
            column [ spacing 8 ]
                [ viewUnit "Final Stock" (State.stockOnHand s)
                , viewMoney "Fiat Balance" (State.fiatBalance s)
                , viewMoney "CC Balance" (State.ccBalance s)
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
    column [ paddingXY 12 8, spacing 8, width (px 800), alignTop, Background.color Style.lightColor ]
        (List.map viewConfigItem (Config.stringValue model.config))


viewHistory_ model =
    column [ centerX, alignTop, spacing 5, padding 20, Background.color Style.charcoal, width (px 800) ]
        [ column [ width (px 770), scrollbarX ] (viewHistory model.history)
        , row [ paddingEach { emptyPadding | top = 10 }, spacing 12 ] [ resetButton, stepButton, runButton ]
        , parameters1 model
        , parameters2 model
        , row [ paddingEach { emptyPadding | top = 20 } ] [ legend ]
        ]


parameters1 model =
    row [ spacing 12, paddingEach { emptyPadding | top = 20 }, Font.color Style.whiteColor ]
        [ textField EnterInitialFiatBalance model.initialFiatBalanceAsString "Initial Fiat bal."
        , textField EnterInitialStock model.initialStockAsString "Initial Stock"
        , textField EnterCCEarnings model.ccEarningsAsString "CC earned"
        , textField EnterCCRatio model.ccRatioAsString "CC Ratio"
        , textField EnterLowOrder model.lowOrderAsString "Low order"
        , textField EnterHighOrder model.highOrderAsString "High order"
        , textField EnterThreshold model.thresholdAsString "Order threshold"
        ]


parameters2 model =
    row [ spacing 12, paddingEach { emptyPadding | top = 20 }, Font.color Style.whiteColor ]
        [ textField EnterCycleLength model.cycleLengthAsString "Cycle length"
        , textField EnterSeed model.initialSeedAsString "Seed"
        , textField EnterDemandMean model.demandMean "Demand mean"
        , textField EnterDemandSpread model.demandSpread "Demand spread"
        ]


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
        ]


legend1 =
    row [ spacing 8, Font.color Style.lightColor ]
        (List.map legendFormatter legendItems1)


legendItems1 =
    [ "T: time"
    , "|"
    , "CC: CC balance"
    , "|"
    , "ST: Stock"
    , "|"
    , "OL: Orders lost"
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
        -- |> List.indexedMap (\k r -> showIf (k /= 1 && k /= 4) (row [ bg k, spacing 4, padding 4 ] r))
        |> List.indexedMap (\k r -> showIf True (row [ bg k, spacing 4, padding 4 ] r))


showIf : Bool -> Element msg -> Element msg
showIf condition element =
    if condition then
        element

    else
        Element.none


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


textField msg text label =
    TextField.make msg text label
        |> TextField.withHeight 30
        |> TextField.withLabelPosition LabelAbove
        |> TextField.toElement
