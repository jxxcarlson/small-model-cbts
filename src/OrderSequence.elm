module OrderSequence exposing
    ( OrderSequence
    , fromList
    , timeOf
    , unConsAt
    , zero
    )

import Fundamental exposing (Time, Units)
import Order exposing (ItemOrder)


type OrderSequence
    = OrderSequence { orders : List ItemOrder }


zero : OrderSequence
zero =
    OrderSequence { orders = [] }


timeOf : OrderSequence -> Time
timeOf (OrderSequence data) =
    List.head data.orders
        |> Maybe.map Order.timeOf
        |> Maybe.withDefault 0


fromList : List ( Time, Units ) -> OrderSequence
fromList list =
    let
        orders =
            list
                |> List.sortBy (\( t, u ) -> t)
                |> List.map (\( t, u ) -> Order.initFromPair ( t, u ))
    in
    OrderSequence { orders = orders }


push : Time -> Units -> OrderSequence -> OrderSequence
push t u ((OrderSequence data) as orderSequence) =
    case t >= timeOf orderSequence of
        False ->
            orderSequence

        True ->
            OrderSequence { data | orders = Order.init t u :: data.orders }


unConsAt : Time -> OrderSequence -> ( Maybe ItemOrder, OrderSequence )
unConsAt t ((OrderSequence data) as orderSequence) =
    case t /= timeOf orderSequence of
        True ->
            ( Nothing, orderSequence )

        False ->
            let
                h =
                    List.head data.orders
            in
            case h of
                Nothing ->
                    ( Nothing, orderSequence )

                Just order ->
                    ( Just order, OrderSequence { orders = List.drop 1 data.orders } )
