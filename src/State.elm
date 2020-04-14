module State exposing
    ( State
    , addToFiatBalance
    , addToStock
    , fiatBalance
    , fillCustomerOrder
    , incrementTime
    , initial
    , initialState
    , labels
    , orderSupplies
    , ordersFilled
    , ordersLost
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
import Order exposing (ItemOrder)
import OrderSequence exposing (Future)
import Random
import Unit.Money as Money exposing (Money)
import Unit.Time as Time exposing (Time)
import Unit.Unit as Unit exposing (Unit, UnitCost)


type State
    = State
        { t : Time
        , fiatBalance : Money
        , ccBalance : Money
        , stock : Unit
        , ordersFilled : Unit
        , ordersLost : Unit
        , seed : Random.Seed
        , messages : Messages
        , orderPlaced : Unit
        , customerOrder : Unit
        , businessOrder : Unit
        }


update : ItemOrder -> State -> State
update itemOrder state =
    state
        |> orderSupplies
        |> fillCustomerOrder itemOrder
        |> incrementTime


labels : List String
labels =
    [ "t", "fi", "cc", "st", "of", "ol", "OP", "CO", "BO" ]


stringVal : State -> List String
stringVal (State data) =
    [ Time.stringVal data.t
    , Money.stringVal data.fiatBalance
    , Money.stringVal data.ccBalance
    , Unit.stringVal data.stock
    , Unit.stringVal data.ordersFilled
    , Unit.stringVal data.ordersLost
    , Unit.stringVal data.orderPlaced
    , Unit.stringVal data.customerOrder
    , Unit.stringVal data.businessOrder
    ]


initialState : State
initialState =
    initial
        |> addToStock (Unit.create 10)


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
        , messages = Message.init 5
        , orderPlaced = Unit.create 0
        , customerOrder = Unit.create 0
        , businessOrder = Unit.create 0
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
                    Money.min config.ccOrderMax (ccBalance state)

                ccOrderAmount : Money
                ccOrderAmount =
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
                    , businessOrder = actualOrderAmount
                }


{-| -}
fillCustomerOrder : ItemOrder -> State -> State
fillCustomerOrder itemOrder ((State data) as state) =
    let
        currentOrder : Unit
        currentOrder =
            Order.unitsOf itemOrder

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
            Unit.add (ordersLost state) currentOrderLoss

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
            , orderPlaced = currentOrder
            , customerOrder = actualOrder
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
