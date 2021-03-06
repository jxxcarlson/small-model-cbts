module Example exposing (..)

import Expect exposing (Expectation)
import Future exposing (Future)
import Fuzz exposing (Fuzzer, int, list, string)
import State exposing (State)
import Test exposing (..)
import Unit.Money as Money
import Unit.Time as Time
import Unit.Unit as Unit


{-| Attempt to buy 8 units at time t >= 2
-}
orderSequence1 : Future
orderSequence1 =
    Future.fromList [ ( 0, 2 ), ( 3, 5 ) ]


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
                        State.fillCustomerOrder initialState

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
                            |> State.fillCustomerOrder
                            |> State.incrementTime
                            |> State.fillCustomerOrder
                            |> State.incrementTime
                            |> State.fillCustomerOrder

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
                            |> State.fillCustomerOrder
                            |> State.incrementTime
                            |> State.fillCustomerOrder
                            |> State.incrementTime
                            |> State.fillCustomerOrder
                            |> State.incrementTime
                            |> State.fillCustomerOrder

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
                        State.orderSupplies initialState1

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
                        State.orderSupplies initialState1

                    st =
                        State.stockOnHand newState

                    fb =
                        State.fiatBalance newState
                in
                Expect.equal ( st, fb ) ( Unit.create 10, Money.create 0 )
        ]
