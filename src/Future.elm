module Future exposing
    ( Future
    , fromList
    , futureV0
    , futureV1
    , generateListWithMean
    , head
    , length
    , stringVal
    , tail
    , take
    , timeValueOf
    , unConsAt
    , zero
    )

import Generate
import Order exposing (ItemOrder)
import Unit.Time as Time exposing (Time)
import Unit.Unit as Unit exposing (Unit)


generateListWithMean : Int -> Int -> Int -> Int -> Future
generateListWithMean seed mean_ spread n =
    let
        seq =
            List.indexedMap (\k item -> ( k, item )) (Generate.positiveListWithMean seed n mean_ spread)
    in
    fromList seq


{-| Attempt to buy 8 units at time t >= 2
-}
futureV1 : Future
futureV1 =
    fromList
        [ ( 0, 2 )
        , ( 1, 2 )
        , ( 2, 0 )
        , ( 3, 5 )
        , ( 4, 0 )
        , ( 5, 10 )
        , ( 6, 4 )
        , ( 7, 0 )
        , ( 8, 14 )
        , ( 9, 5 )
        , ( 10, 2 )
        , ( 11, 3 )
        , ( 12, 4 )
        , ( 14, 11 )
        , ( 15, 2 )
        , ( 16, 0 )
        , ( 17, 15 )
        , ( 18, 0 )
        , ( 19, 1 )
        , ( 20, 15 )
        , ( 21, 5 )
        , ( 22, 0 )
        , ( 23, 8 )
        , ( 24, 0 )
        , ( 25, 2 )
        , ( 26, 16 )
        , ( 27, 0 )
        , ( 28, 4 )
        , ( 29, 15 )
        ]


futureV0 : Future
futureV0 =
    fromList
        [ ( 0, 5 )
        , ( 1, 5 )
        , ( 2, 5 )
        , ( 3, 5 )
        , ( 4, 5 )
        , ( 5, 5 )
        , ( 6, 5 )
        , ( 7, 5 )
        , ( 8, 5 )
        , ( 9, 5 )
        , ( 10, 5 )
        , ( 11, 5 )
        , ( 12, 5 )
        , ( 14, 5 )
        , ( 15, 5 )
        , ( 16, 5 )
        , ( 17, 5 )
        , ( 18, 5 )
        , ( 19, 5 )
        , ( 20, 5 )
        , ( 21, 5 )
        , ( 22, 5 )
        , ( 23, 5 )
        , ( 24, 5 )
        , ( 25, 5 )
        , ( 26, 5 )
        , ( 27, 5 )
        , ( 28, 5 )
        , ( 29, 5 )
        ]


orderSequence2 : Future
orderSequence2 =
    fromList [ ( 0, 2 ), ( 3, 5 ) ]


type Future
    = OrderSequence { orders : List ItemOrder }


length : Future -> Int
length (OrderSequence data) =
    List.length data.orders


head : Future -> Maybe ItemOrder
head (OrderSequence data) =
    List.head data.orders


tail : Future -> Future
tail (OrderSequence data) =
    OrderSequence { data | orders = List.tail data.orders |> Maybe.withDefault [] }


zero : Future
zero =
    OrderSequence { orders = [] }


stringVal : Future -> String
stringVal (OrderSequence data) =
    List.map Order.stringValue data.orders
        |> String.join ", "


take : Int -> Future -> Future
take k (OrderSequence data) =
    OrderSequence { orders = List.take k data.orders }


timeValueOf : Future -> Time
timeValueOf (OrderSequence data) =
    List.head data.orders
        |> Maybe.map Order.timeOf
        |> Maybe.withDefault Time.zero


fromList : List ( Int, Int ) -> Future
fromList list =
    let
        orders =
            list
                |> List.sortBy (\( t, u ) -> t)
                |> List.map (\( t, u ) -> Order.create t u)
    in
    OrderSequence { orders = orders }


push : Time -> Unit -> Future -> Future
push t u ((OrderSequence data) as orderSequence) =
    case Time.gte t (timeValueOf orderSequence) of
        False ->
            orderSequence

        True ->
            OrderSequence { data | orders = Order.init t u :: data.orders }


unConsAt : Time -> Future -> ( Maybe ItemOrder, Future )
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
