module OrderSequence exposing
    ( OrderSequence
    , fromList
    , timeValueOf
    , unConsAt
    , zero
    )

import Order exposing (ItemOrder)
import Unit.Time as Time exposing (Time)
import Unit.Unit as Unit exposing (Unit)


orderSequence2 : OrderSequence
orderSequence2 =
    fromList [ ( 0, 2 ), ( 3, 5 ) ]


type OrderSequence
    = OrderSequence { orders : List ItemOrder }


zero : OrderSequence
zero =
    OrderSequence { orders = [] }


timeValueOf : OrderSequence -> Time
timeValueOf (OrderSequence data) =
    List.head data.orders
        |> Maybe.map Order.timeOf
        |> Maybe.withDefault Time.zero


fromList : List ( Int, Int ) -> OrderSequence
fromList list =
    let
        orders =
            list
                |> List.sortBy (\( t, u ) -> t)
                |> List.map (\( t, u ) -> Order.create t u)
    in
    OrderSequence { orders = orders }


push : Time -> Unit -> OrderSequence -> OrderSequence
push t u ((OrderSequence data) as orderSequence) =
    case Time.gte t (timeValueOf orderSequence) of
        False ->
            orderSequence

        True ->
            OrderSequence { data | orders = Order.init t u :: data.orders }


unConsAt : Time -> OrderSequence -> ( Maybe ItemOrder, OrderSequence )
unConsAt t ((OrderSequence data) as orderSequence) =
    case t /= timeValueOf orderSequence of
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
