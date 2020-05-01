module Unit.Money exposing
    ( Money
    , add
    , create
    , divBy
    , divByInt
    , min
    , mulBy
    , mulByInt
    , roundTo
    , stringVal
    , sub
    , value
    )


type Money
    = Money Float


create : Float -> Money
create m =
    Money m


value : Money -> Float
value (Money k) =
    k


stringVal : Money -> String
stringVal (Money k) =
    String.fromFloat k


add : Money -> Money -> Money
add x y =
    map2 (+) x y


sub : Money -> Money -> Money
sub x y =
    map2 (-) x y


mulBy : Float -> Money -> Money
mulBy k m =
    map (\x -> k * x) m


mulByInt : Int -> Money -> Money
mulByInt k m =
    map (\x -> toFloat k * x) m


divBy : Float -> Money -> Money
divBy k m =
    map (\x -> x / k) m


divByInt : Int -> Money -> Money
divByInt k m =
    map (\x -> x / toFloat k) m


roundTo_ : Int -> Float -> Float
roundTo_ k x =
    let
        factor =
            10 ^ toFloat k
    in
    toFloat (round (factor * x)) / factor


roundTo : Int -> Money -> Money
roundTo k m =
    map (roundTo_ k) m


min : Money -> Money -> Money
min a b =
    map2 Basics.min a b


map : (Float -> Float) -> Money -> Money
map f (Money x) =
    Money (f x)


map2 : (Float -> Float -> Float) -> Money -> Money -> Money
map2 f (Money x) (Money y) =
    Money (f x y)
