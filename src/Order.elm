module Order exposing (ItemOrder, create, init, initFromPair, timeOf, unitsOf)

import Unit.Time as Time exposing (Time)
import Unit.Unit as Item exposing (Unit)


type ItemOrder
    = Order ( Time, Unit )


create : Int -> Int -> ItemOrder
create time items =
    Order ( Time.create time, Item.create items )


timeOf : ItemOrder -> Time
timeOf (Order ( t, u )) =
    t


initFromPair : ( Time, Unit ) -> ItemOrder
initFromPair ( t, u ) =
    Order ( t, u )


unitsOf : ItemOrder -> Unit
unitsOf (Order ( t, u )) =
    u


init : Time -> Unit -> ItemOrder
init t u =
    Order ( t, u )
