module State exposing
    ( State
    , addToFiatBalance
    , addToStock
    , fiatBalance
    , fillCustomerOrder
    , incrementTime
    , initial
    , initialState
    , orderSequence1
    , orderSupplies
    , ordersFilled
    , ordersLost
    , setCustomerOrders
    , setTime
    , stockOnHand
    , stringVal
    , timeOf
    , update
    )

{-| The State in this model refers to the state of
business (pulsero) which sells a single item to the public
and which purchase its stock from a single supplier.
The state contains fields such as "stock," which
is the inventory, denominated in Units, as well
as orders filled, orders lost (not filled because
of insufficient supply), etc.

    Values such as unitCost (cost of the item to the
    pulsero) and unitPrice (cost of the item to the public)
    are defined in config.

    There are two main functions:

        - fillCustomerOrder : Time -> State -> State
        - orderSupplies : Time -> State -> State

-}

import Message exposing (Messages)
import Order
import OrderSequence exposing (OrderSequence)
import Random
import Unit.Money as Money exposing (Money)
import Unit.Time as Time exposing (Time)
import Unit.Unit as Unit exposing (Unit, UnitCost)


{-| Attempt to buy 8 units at time t >= 2
-}
orderSequence1 : OrderSequence
orderSequence1 =
    OrderSequence.fromList
        [ ( 0, 2 )
        , ( 1, 2 )
        , ( 3, 5 )
        , ( 5, 10 )
        , ( 6, 4 )
        , ( 8, 14 )
        , ( 9, 5 )
        , ( 10, 2 )
        , ( 11, 3 )
        , ( 12, 4 )
        , ( 14, 11 )
        , ( 15, 2 )
        , ( 17, 20 )
        , ( 19, 1 )
        , ( 20, 15 )
        , ( 21, 5 )
        , ( 23, 8 )
        , ( 25, 2 )
        , ( 26, 16 )
        , ( 28, 4 )
        , ( 29, 15 )
        ]


type State
    = State
        { t : Time
        , fiatBalance : Money
        , ccBalance : Money
        , stock : Unit
        , ordersFilled : Unit
        , ordersLost : Unit
        , seed : Random.Seed
        , orderSequence : OrderSequence
        , messages : Messages
        }


update : State -> State
update state =
    state
        |> orderSupplies
        |> fillCustomerOrder
        |> incrementTime


stringVal : State -> List { label : String, value : String }
stringVal (State data) =
    [ { label = "t", value = Time.stringVal data.t }
    , { label = "fiat", value = Money.stringVal data.fiatBalance }
    , { label = "cc", value = Money.stringVal data.ccBalance }
    , { label = "stock", value = Unit.stringVal data.stock }
    , { label = "ordersFilled", value = Unit.stringVal data.ordersFilled }
    , { label = "ordersLost", value = Unit.stringVal data.ordersLost }
    , { label = "orderSequence", value = OrderSequence.stringVal <| OrderSequence.take 5 data.orderSequence }
    , { label = "messages", value = Message.stringVal 6 data.messages }
    ]


initialState : State
initialState =
    initial
        |> addToStock (Unit.create 10)
        |> setCustomerOrders orderSequence1


orderSequenceOf : State -> OrderSequence
orderSequenceOf (State data) =
    data.orderSequence


initial : State
initial =
    State
        { t = Time.create 0
        , fiatBalance = Money.create 0
        , ccBalance = Money.create 0
        , stock = Unit.create 0
        , ordersFilled = Unit.create 0
        , ordersLost = Unit.create 0
        , seed = Random.initialSeed 1234
        , orderSequence = OrderSequence.zero
        , messages = Message.init 5
        }


type alias Config =
    { unitCost : UnitCost
    , unitPrice : UnitCost
    , stockOnHandThreshold : Unit
    , lowOrder : Unit
    , highOrder : Unit
    , ccOrderMax : Money
    }


config : Config
config =
    { unitCost = Unit.unitCost 1.0
    , unitPrice = Unit.unitCost 2.0
    , stockOnHandThreshold = Unit.create 10
    , lowOrder = Unit.create 5
    , highOrder = Unit.create 20
    , ccOrderMax = Money.create 5
    }


{-|

    Steps to determine order data
    1. Choose a random orderQuantity, then compute orderCost
    2. Based on current fiat and cc account balances, determine
       orderAmount from each account
    3. The actual

-}
orderSupplies : State -> State
orderSupplies ((State data) as state) =
    let
        t_ =
            timeOf state
    in
    case Unit.lt (stockOnHand state) config.stockOnHandThreshold of
        False ->
            state

        True ->
            let
                ( orderQuantity_, newSeed ) =
                    Random.step (Random.int (Unit.value config.lowOrder) (Unit.value config.highOrder)) (seedOf state)

                orderQuantity =
                    Unit.create orderQuantity_

                orderCost =
                    Unit.costOf config.unitCost orderQuantity

                ccAvailable =
                    Debug.log "ccAvailable" <|
                        Money.min config.ccOrderMax (ccBalance state)

                ccOrderAmount : Money
                ccOrderAmount =
                    Debug.log "ccOrderAmount" <|
                        Money.min ccAvailable orderCost

                fiatOrderAmount : Money
                fiatOrderAmount =
                    Money.min (fiatBalance state) (Money.sub orderCost ccOrderAmount)

                actualOrderAmount =
                    let
                        totalOrder =
                            Money.add ccOrderAmount fiatOrderAmount
                    in
                    Unit.itemsFor config.unitCost totalOrder

                message =
                    "ORDER (t, fiat, cc)    : ("
                        ++ Time.stringVal (timeOf state)
                        ++ ", "
                        ++ Money.stringVal fiatOrderAmount
                        ++ ", "
                        ++ Money.stringVal ccOrderAmount
                        ++ ")"
            in
            State
                { data
                    | seed = newSeed
                    , ccBalance = Money.sub (ccBalance state) ccOrderAmount
                    , fiatBalance = Money.sub (fiatBalance state) fiatOrderAmount
                    , stock = Unit.add (stockOnHand state) actualOrderAmount
                    , ordersFilled = Unit.add actualOrderAmount (ordersFilled state)
                    , ordersLost = Unit.add (Unit.sub orderQuantity actualOrderAmount) (ordersLost state)
                    , messages = Message.push message data.messages
                }


{-| -}
fillCustomerOrder : State -> State
fillCustomerOrder ((State data) as state) =
    let
        ( maybeCurrentOrder, newOrderSequence ) =
            OrderSequence.unConsAt (timeOf state) (orderSequenceOf state)
    in
    case maybeCurrentOrder of
        Nothing ->
            state

        Just currentOrder_ ->
            let
                currentOrder : Unit
                currentOrder =
                    Order.unitsOf currentOrder_

                actualOrder =
                    Unit.min currentOrder (stockOnHand state)

                fiatBalance_ =
                    let
                        orderPrice =
                            Unit.costOf config.unitPrice actualOrder
                    in
                    Money.add (fiatBalance state) orderPrice

                ordersFilled_ =
                    Unit.add (ordersFilled state) actualOrder

                currentOrderLoss =
                    Unit.sub currentOrder actualOrder

                ordersLost_ =
                    Unit.sub (ordersLost state) currentOrderLoss

                message =
                    "BUY (t, filled, lost) = ("
                        ++ Time.stringVal (timeOf state)
                        ++ ", "
                        ++ Unit.stringVal ordersFilled_
                        ++ ", "
                        ++ Unit.stringVal ordersLost_
                        ++ ")"
            in
            State
                { data
                    | fiatBalance = fiatBalance_
                    , ordersFilled = ordersFilled_
                    , ordersLost = ordersLost_
                    , stock = Unit.sub (stockOnHand state) actualOrder
                    , orderSequence = newOrderSequence
                    , messages = Message.push message data.messages
                }



-- GETTERS


timeOf : State -> Time
timeOf (State data) =
    data.t


seedOf : State -> Random.Seed
seedOf (State data) =
    data.seed


stockOnHand : State -> Unit
stockOnHand (State data) =
    data.stock


fiatBalance : State -> Money
fiatBalance (State data) =
    data.fiatBalance


ccBalance : State -> Money
ccBalance (State data) =
    data.ccBalance


ordersFilled : State -> Unit
ordersFilled (State data) =
    data.ordersFilled


ordersLost : State -> Unit
ordersLost (State data) =
    data.ordersLost



-- SETTERS


setTime : Time -> State -> State
setTime t (State data) =
    State { data | t = t }


incrementTime : State -> State
incrementTime (State data) =
    State { data | t = Time.increment data.t }


addToFiatBalance : Money -> State -> State
addToFiatBalance fc (State data) =
    State { data | fiatBalance = Money.add fc data.fiatBalance }


setCustomerOrders : OrderSequence -> State -> State
setCustomerOrders orderSequence (State data) =
    State { data | orderSequence = orderSequence }


addToStock : Unit -> State -> State
addToStock units (State data) =
    State { data | stock = Unit.add data.stock units }



-- NOT USED


mapWithState : (s -> a -> ( s, a )) -> ( s, List a ) -> ( s, List a )
mapWithState f ( state, list ) =
    let
        folder : a -> ( s, List a ) -> ( s, List a )
        folder item ( state_, list_ ) =
            let
                ( newState_, item_ ) =
                    f state_ item
            in
            ( newState_, item_ :: list_ )
    in
    List.foldl folder ( state, [] ) list
