module State exposing
    ( State
    , addToFiatBalance
    , addToStock
    , ccBalance
    , fiatBalance
    , fillCustomerOrder
    , getBusinessOrder
    , getDemand
    , getOrdersLost
    , incrementTime
    , initial
    , initialState
    , initialStateWithConfig
    , initialStock
    , labels
    , last
    , log
    , orderSupplies
    , ordersFilled
    , ordersLost
    , setTime
    , stockOnHand
    , stringVal
    , sumRowOfUnits
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

import Config exposing (Config, default)
import Future exposing (Future)
import List.Extra
import Maybe.Extra
import Message exposing (Messages)
import Order exposing (ItemOrder)
import Random
import Unit.Money as Money exposing (Money)
import Unit.Time as Time exposing (Time)
import Unit.Unit as Unit exposing (Unit, UnitCost)


type State
    = State
        { t : Time
        , fiatBalance : Money
        , ccBalance : Money
        , ccRatio : Float
        , stock : Unit
        , ordersFilled : Unit
        , ordersLost : Unit
        , seed : Random.Seed
        , orderPlaced : Unit
        , customerOrder : Unit
        , businessOrder : Unit
        , log : List ( Time, String, String )
        }


getDemand : State -> Unit
getDemand (State data) =
    data.orderPlaced


getBusinessOrder : State -> Unit
getBusinessOrder (State data) =
    data.businessOrder


getOrdersLost : State -> Unit
getOrdersLost (State data) =
    data.ordersLost


last : List State -> State
last listOfStates =
    listOfStates
        |> List.head
        |> Maybe.withDefault initialState


first : List State -> State
first listOfStates =
    listOfStates
        |> List.reverse
        |> List.head
        |> Maybe.withDefault initialState


initialStock listOfStates =
    listOfStates
        |> first
        |> stockOnHand


update : Config -> ItemOrder -> State -> State
update config itemOrder state =
    state
        |> orderSupplies2 config
        --|> orderSuppliesWithCC config
        |> fillCustomerOrder config itemOrder
        |> incrementTime


labels : List ( String, String )
labels =
    [ ( "T", "Time step" )
    , ( "FC", "Fiat currency (balance)" )
    , ( "CC", "Complementary currency (balance)" )
    , ( "ST", "Stock" )
    , ( "OF", "Orders filled" )
    , ( "OL", "Orders lost" )
    , ( "CD", "Customer demand" )
    , ( "CB", "Customer bought" )
    , ( "OS", "Orders to suppliers" )
    ]


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


initialStateWithConfig : Config -> State
initialStateWithConfig c =
    initial
        |> addToStock c.initialStock
        |> addToFiatBalance c.initialFiatBalance
        |> addToCCBalance c.initialCCBalance
        |> setCCRatio c.ccRatio


initial : State
initial =
    State
        { t = Time.create 0
        , fiatBalance = Money.create 0
        , ccBalance = Money.create 0
        , ccRatio = 0
        , stock = Unit.create 0
        , ordersFilled = Unit.create 0
        , ordersLost = Unit.create 0
        , seed = Random.initialSeed 1234
        , orderPlaced = Unit.create 0
        , customerOrder = Unit.create 0
        , businessOrder = Unit.create 0
        , log = []
        }


{-|

    Steps to determine order data
    1. Choose a random orderQuantity, then compute orderCost
    2. Based on current fiat and cc account balances, determine
       orderAmount from each account
    3. The actual

-}
orderSupplies : Config -> State -> State
orderSupplies config ((State data) as state) =
    let
        t_ =
            timeOf state
    in
    case Unit.lt (stockOnHand state) config.stockOnHandThreshold of
        False ->
            State
                { data | businessOrder = Unit.create 0 }

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
                    ( timeOf state, message_, "" )

                message_ =
                    "ORDER "
                        ++ Money.stringVal fiatOrderAmount
                        ++ ", "
                        ++ Money.stringVal ccOrderAmount
                        ++ ", "
                        ++ Unit.stringVal actualOrderAmount
            in
            State
                { data
                    | seed = newSeed
                    , ccBalance = Money.sub (ccBalance state) ccOrderAmount
                    , fiatBalance = Money.sub (fiatBalance state) fiatOrderAmount
                    , stock = Unit.add (stockOnHand state) actualOrderAmount
                    , businessOrder = actualOrderAmount
                    , log = message :: data.log
                }


orderSupplies2 : Config -> State -> State
orderSupplies2 config ((State data) as state) =
    let
        t_ =
            timeOf state
    in
    case Unit.lt (stockOnHand state) config.stockOnHandThreshold of
        False ->
            State
                { data | businessOrder = Unit.create 0 }

        True ->
            let
                ( orderQuantity_, newSeed ) =
                    Random.step (Random.int (Unit.value config.lowOrder) (Unit.value config.highOrder)) (seedOf state)

                orderQuantity =
                    Unit.create orderQuantity_

                orderCost =
                    Unit.costOf config.unitCost orderQuantity

                ccAvailable =
                    ccBalance state

                ccOrderAmount : Money
                ccOrderAmount =
                    Money.min ccAvailable (Money.mulBy data.ccRatio orderCost |> Money.roundTo 0)

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
                    ( timeOf state, message_, "" )

                message_ =
                    "ORDER "
                        ++ Money.stringVal fiatOrderAmount
                        ++ ", "
                        ++ Money.stringVal ccOrderAmount
                        ++ ", "
                        ++ Unit.stringVal actualOrderAmount
            in
            State
                { data
                    | seed = newSeed
                    , ccBalance = Money.sub (ccBalance state) ccOrderAmount
                    , fiatBalance = Money.sub (fiatBalance state) fiatOrderAmount
                    , stock = Unit.add (stockOnHand state) actualOrderAmount
                    , businessOrder = actualOrderAmount
                    , log = message :: data.log
                }


orderSuppliesWithCC : Config -> State -> State
orderSuppliesWithCC config ((State data) as state) =
    case Unit.lt (stockOnHand state) (Unit.create 10) of
        True ->
            orderSuppliesWithCC_ config state

        False ->
            state


orderSuppliesWithCC_ : Config -> State -> State
orderSuppliesWithCC_ config ((State data) as state) =
    let
        t_ =
            timeOf state
    in
    let
        ccAvailable =
            ccBalance state

        ccForOrder =
            Money.min (Money.create 100) ccAvailable

        ccOrderAmount =
            Unit.itemsFor config.unitCost ccForOrder

        message =
            ( timeOf state, message_, "" )

        message_ =
            if Unit.gt ccOrderAmount (Unit.create 0) then
                "CC ORDER "
                    ++ Money.stringVal ccForOrder
                    ++ ", "
                    ++ Unit.stringVal ccOrderAmount

            else
                ""
    in
    State
        { data
            | ccBalance = Money.sub (ccBalance state) ccForOrder
            , stock = Unit.add (stockOnHand state) ccOrderAmount
            , businessOrder = Unit.add ccOrderAmount (getBusinessOrder state)
            , log = message :: data.log
        }


{-| -}
fillCustomerOrder : Config -> ItemOrder -> State -> State
fillCustomerOrder config itemOrder ((State data) as state) =
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

        ordersLostMessage =
            case Unit.value currentOrderLoss /= 0 of
                True ->
                    "LOST: " ++ Unit.stringVal currentOrderLoss

                False ->
                    ""

        message =
            ( timeOf state, message_, ordersLostMessage )

        message_ =
            "BUY " ++ Unit.stringVal actualOrder
    in
    State
        { data
            | fiatBalance = fiatBalance_
            , ordersFilled = ordersFilled_
            , ordersLost = ordersLost_
            , stock = Unit.sub (stockOnHand state) actualOrder
            , orderPlaced = currentOrder
            , customerOrder = actualOrder
            , log = message :: data.log
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


log : State -> List ( Time, String, String )
log (State data) =
    data.log



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


setCCRatio : Float -> State -> State
setCCRatio ccRatio (State data) =
    State { data | ccRatio = ccRatio }


addToCCBalance : Money -> State -> State
addToCCBalance fc (State data) =
    State { data | ccBalance = Money.add fc data.ccBalance }


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


getRowOfUnits : (State -> Unit) -> List State -> List Unit
getRowOfUnits getField stateList =
    List.map getField stateList


sumRowOfUnits : (State -> Unit) -> List State -> Unit
sumRowOfUnits getField stateList =
    List.foldl Unit.add (Unit.create 0) (getRowOfUnits getField stateList)



--  |> Maybe.Extra.valuesnitRowSummary : Int -> List State -> Unit
--unitRowSummary row stateList =
--   let
--      adder : state -> Unit -> Unit
--      adder state u =
--          (List.Extra.getAt row state |> )
--   in
--   List.foldl adder (Unit.create 0) stateList
