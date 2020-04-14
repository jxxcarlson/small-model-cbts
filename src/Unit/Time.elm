module Unit.Time exposing
    ( Time
    , create
    , gte
    , increment
    , stringVal
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


stringVal : Time -> String
stringVal (Time k) =
    String.fromInt k


increment : Time -> Time
increment (Time k) =
    Time (k + 1)


gte : Time -> Time -> Bool
gte s t =
    map2 (>=) s t


map2 : (Int -> Int -> a) -> Time -> Time -> a
map2 f (Time x) (Time y) =
    f x y
