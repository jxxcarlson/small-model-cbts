module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import OrderSequence exposing (OrderSequence)
import State exposing (State)
import Test exposing (..)


{-| Attempt to buy 8 units at time t >= 2
-}
orderSequence1 : OrderSequence
orderSequence1 =
    OrderSequence.fromList [ ( 0, 2 ), ( 3, 5 ) ]


{-| 10 units on hand on day 0
-}
initialState : State
initialState =
    State.initial
        |> State.addToStock 10
        |> State.setCustomerOrders orderSequence1


initialState2 : State
initialState2 =
    State.initial
        |> State.addToFiatBalance 40


suite : Test
suite =
    describe "The State module"
        [ test "fill an order at time 0 (there is no order standing at time 0" <|
            \_ ->
                let
                    newState =
                        State.fillCustomerOrderAt 0 initialState

                    stock =
                        State.stockOnHand newState

                    fb =
                        State.fiatBalance newState
                in
                Expect.equal ( stock, fb ) ( 8, 4 )
        , test "attempt to fill orders at times 0, 1, 2" <|
            \_ ->
                let
                    newState =
                        initialState
                            |> State.fillCustomerOrderAt 0
                            |> State.fillCustomerOrderAt 1
                            |> State.fillCustomerOrderAt 2

                    stock =
                        State.stockOnHand newState

                    fb =
                        State.fiatBalance newState
                in
                Expect.equal ( stock, fb ) ( 8, 4 )
        , test "attempt to fill orders at times 0, 1, 2, 3" <|
            \_ ->
                let
                    newState =
                        initialState
                            |> State.fillCustomerOrderAt 0
                            |> State.fillCustomerOrderAt 3

                    stock =
                        State.stockOnHand newState

                    fb =
                        State.fiatBalance newState
                in
                Expect.equal ( stock, fb ) ( 3, 14 )
        , test "set up to buy supplies" <|
            \_ ->
                Expect.equal (State.fiatBalance initialState2) 40
        , only <|
            test "Buy supplies" <|
                \_ ->
                    let
                        initialState1 =
                            State.initial
                                |> State.addToFiatBalance 40

                        newState =
                            Debug.log "newState" <|
                                State.orderSupplies 0 initialState1

                        st =
                            State.stockOnHand newState

                        fb =
                            State.fiatBalance newState

                        orf =
                            State.ordersFilled newState

                        lost =
                            State.ordersLost newState
                    in
                    Expect.equal ( ( st, fb ), ( orf, lost ) ) ( ( 20, 20 ), ( 20, 0 ) )
        , test "Buy supplies with smaller fiat balance" <|
            \_ ->
                let
                    initialState1 =
                        State.initial
                            |> State.addToFiatBalance 10

                    newState =
                        Debug.log "newState" <|
                            State.orderSupplies 0 initialState1

                    st =
                        State.stockOnHand newState

                    fb =
                        State.fiatBalance newState
                in
                Expect.equal ( st, fb ) ( 20, 20 )
        ]
