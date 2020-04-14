module OrderSequence exposing
    ( OrderSequence
    , fromList
    , head
    , orderSequence1
    , stringVal
    , tail
    , take
    , timeValueOf
    , unConsAt
    , zero
    )

import Order exposing (ItemOrder)
import Unit.Time as Time exposing (Time)
import Unit.Unit as Unit exposing (Unit)


{-| Attempt to buy 8 units at time t >= 2
-}
orderSequence1 : OrderSequence
orderSequence1 =
    fromList
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


orderSequence2 : OrderSequence
orderSequence2 =
    fromList [ ( 0, 2 ), ( 3, 5 ) ]


type OrderSequence
    = OrderSequence { orders : List ItemOrder }


head : OrderSequence -> Maybe ItemOrder
head (OrderSequence data) =
    List.head data.orders


tail : OrderSequence -> OrderSequence
tail (OrderSequence data) =
    OrderSequence { data | orders = List.tail data.orders |> Maybe.withDefault [] }


zero : OrderSequence
zero =
    OrderSequence { orders = [] }


stringVal : OrderSequence -> String
stringVal (OrderSequence data) =
    List.map Order.stringValue data.orders
        |> String.join ", "


take : Int -> OrderSequence -> OrderSequence
take k (OrderSequence data) =
    OrderSequence { orders = List.take k data.orders }


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
