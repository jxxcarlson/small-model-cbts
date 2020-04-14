module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import OrderSequence exposing (OrderSequence)
import State exposing (State)
import Test exposing (..)
import Unit.Money as Money
import Unit.Time as Time
import Unit.Unit as Unit


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
        |> State.addToStock (Unit.create 10)
        |> State.setCustomerOrders orderSequence1


initialState2 : State
initialState2 =
    State.initial
        |> State.addToFiatBalance (Money.create 40)


suite : Test
suite =
    describe "The State module"
        [ test "fill an order at time 0 (there is no order standing at time 0" <|
            \_ ->
                let
                    newState =
                        State.fillCustomerOrderAt (Time.create 0) initialState

                    stock =
                        State.stockOnHand newState

                    fb =
                        State.fiatBalance newState
                in
                Expect.equal ( stock, fb ) ( Unit.create 8, Money.create 4 )
        , test "attempt to fill orders at times 0, 1, 2" <|
            \_ ->
                let
                    newState =
                        initialState
                            |> State.fillCustomerOrderAt (Time.create 0)
                            |> State.fillCustomerOrderAt (Time.create 1)
                            |> State.fillCustomerOrderAt (Time.create 2)

                    stock =
                        State.stockOnHand newState

                    fb =
                        State.fiatBalance newState
                in
                Expect.equal ( stock, fb ) ( Unit.create 8, Money.create 4 )
        , test "attempt to fill orders at times 0, 1, 2, 3" <|
            \_ ->
                let
                    newState =
                        initialState
                            |> State.fillCustomerOrderAt (Time.create 0)
                            |> State.fillCustomerOrderAt (Time.create 3)

                    stock =
                        State.stockOnHand newState

                    fb =
                        State.fiatBalance newState
                in
                Expect.equal ( stock, fb ) ( Unit.create 3, Money.create 14 )
        , test "set up to buy supplies" <|
            \_ ->
                Expect.equal (State.fiatBalance initialState2) (Money.create 40)
        , test "Buy supplies" <|
            \_ ->
                let
                    initialState1 =
                        State.initial
                            |> State.addToFiatBalance (Money.create 40)

                    newState =
                        Debug.log "newState" <|
                            State.orderSupplies (Time.create 0) initialState1

                    st =
                        State.stockOnHand newState

                    fb =
                        State.fiatBalance newState

                    orf =
                        State.ordersFilled newState

                    lost =
                        State.ordersLost newState
                in
                Expect.equal ( ( st, fb ), ( orf, lost ) )
                    ( ( Unit.create 20, Money.create 20 ), ( Unit.create 20, Unit.create 0 ) )
        , test "Buy supplies with smaller fiat balance" <|
            \_ ->
                let
                    initialState1 =
                        State.initial
                            |> State.addToFiatBalance (Money.create 10)

                    newState =
                        Debug.log "newState" <|
                            State.orderSupplies (Time.create 0) initialState1

                    st =
                        State.stockOnHand newState

                    fb =
                        State.fiatBalance newState
                in
                Expect.equal ( st, fb ) ( Unit.create 10, Money.create 0 )
        ]
