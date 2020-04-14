module Unit.Time exposing
    ( Time
    , create
    , gte
    , value
    , zero
    )


type Time
    = Time Int


zero =
    create 0


create : Int -> Time
create k =
    Time k


value : Time -> Int
value (Time k) =
    k


gte : Time -> Time -> Bool
gte s t =
    map2 (>=) s t


map2 : (Int -> Int -> a) -> Time -> Time -> a
map2 f (Time x) (Time y) =
    f x y
