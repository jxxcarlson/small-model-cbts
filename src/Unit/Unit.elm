module Unit.Unit exposing
    ( Unit
    , UnitCost
    , add
    , costOf
    , create
    , itemsFor
    , lt
    , min
    , stringVal
    , sub
    , unitCost
    , value
    )

import Unit.Money as Money exposing (Money)


type Unit
    = Unit Int


type UnitCost
    = UnitCost Float


stringVal : Unit -> String
stringVal (Unit k) =
    String.fromInt k


unitCost : Float -> UnitCost
unitCost c =
    UnitCost c


costOf : UnitCost -> Unit -> Money
costOf (UnitCost costPerItem) (Unit items) =
    Money.create <| toFloat items * costPerItem


itemsFor : UnitCost -> Money -> Unit
itemsFor (UnitCost unitCost_) money =
    create <| floor <| Money.value money / unitCost_


create : Int -> Unit
create k =
    Unit k


value : Unit -> Int
value (Unit k) =
    k


add : Unit -> Unit -> Unit
add ii jj =
    map2 (+) ii jj


sub : Unit -> Unit -> Unit
sub ii jj =
    map2 (-) ii jj


lt : Unit -> Unit -> Bool
lt a b =
    bareMap2 (<) a b


min : Unit -> Unit -> Unit
min a b =
    map2 Basics.min a b


map : (Int -> Int) -> Unit -> Unit
map f (Unit x) =
    Unit (f x)


map2 : (Int -> Int -> Int) -> Unit -> Unit -> Unit
map2 f (Unit x) (Unit y) =
    Unit (f x y)


bareMap2 : (Int -> Int -> a) -> Unit -> Unit -> a
bareMap2 f (Unit x) (Unit y) =
    f x y
