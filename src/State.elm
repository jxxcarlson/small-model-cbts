module State exposing
    ( State
    , addToStock
    , fiatBalance
    , fillCustomerOrderAt
    , initial
    , initialState
    , orderSequence1
    , setCustomerOrders
    , stockOnHand
    )

import Fundamental exposing (Time, Units)
import Order
import OrderSequence exposing (OrderSequence)
import Random


{-| Attempt to buy 8 units at time t >= 2
-}
orderSequence1 : OrderSequence
orderSequence1 =
    OrderSequence.fromList [ ( 0, 2 ), ( 3, 5 ) ]


type State
    = State
        { t : Time
        , fiatBalance : Float
        , ccBalance : Float
        , stock : Units
        , ordersFilled : Units
        , ordersLost : Units
        , seed : Random.Seed
        , orderSequence : OrderSequence
        }


initialState : State
initialState =
    initial
        |> addToStock 10
        |> setCustomerOrders orderSequence1


orderSequenceOf : State -> OrderSequence
orderSequenceOf (State data) =
    data.orderSequence


initial : State
initial =
    State
        { t = 0
        , fiatBalance = 0
        , ccBalance = 0
        , stock = 0
        , ordersFilled = 0
        , ordersLost = 0
        , seed = Random.initialSeed 1234
        , orderSequence = OrderSequence.zero
        }


setCustomerOrders : OrderSequence -> State -> State
setCustomerOrders orderSequence (State data) =
    State { data | orderSequence = orderSequence }


addToStock : Units -> State -> State
addToStock units (State data) =
    State { data | stock = data.stock + units }


type alias Config =
    { unitCost : Float
    , unitPrice : Float
    }


config : Config
config =
    { unitCost = 1.0
    , unitPrice = 2.0
    }


stockOnHand : State -> Units
stockOnHand (State data) =
    data.stock


fiatBalance : State -> Float
fiatBalance (State data) =
    data.fiatBalance


ccBalance : State -> Float
ccBalance (State data) =
    data.ccBalance


ordersFilled : State -> Units
ordersFilled (State data) =
    data.ordersFilled


ordersLost : State -> Units
ordersLost (State data) =
    data.ordersLost


fillCustomerOrderAt : Time -> State -> State
fillCustomerOrderAt t ((State data) as state) =
    let
        ( maybeCurrentOrder, newOrderSequence ) =
            OrderSequence.unConsAt t (orderSequenceOf state)
    in
    case maybeCurrentOrder of
        Nothing ->
            state

        Just currentOrder_ ->
            let
                currentOrder =
                    Order.unitsOf currentOrder_

                actualOrder =
                    min currentOrder (stockOnHand state)

                fiatBalance_ =
                    fiatBalance state
                        + config.unitPrice
                        * toFloat actualOrder

                ordersFilled_ =
                    ordersFilled state
                        + actualOrder

                currentOrderLoss =
                    currentOrder
                        - actualOrder

                ordersLost_ =
                    ordersLost state + currentOrderLoss
            in
            State
                { data
                    | fiatBalance = fiatBalance_
                    , ordersFilled = ordersFilled_
                    , ordersLost = ordersLost_
                    , stock = stockOnHand state - actualOrder
                    , t = t
                    , orderSequence = newOrderSequence
                }



--
--mapWithState : (s -> a -> ( s, a )) -> ( s, List a ) -> ( s, List a )
--mapWithState f ( state, list ) =
--    let
--        folder : a -> ( s, List a ) -> ( s, List a )
--        folder item ( state_, list_ ) =
--            let
--                ( newState_, item_ ) =
--                    f state_ item
--            in
--            ( newState_, item_ :: list_ )
--    in
--    List.foldl folder ( state, [] ) list
