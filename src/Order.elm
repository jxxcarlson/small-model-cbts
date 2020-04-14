module Order exposing (ItemOrder, init, initFromPair, timeOf, unitsOf)

import Fundamental exposing (Time, Units)


type ItemOrder
    = Order ( Time, Units )


timeOf : ItemOrder -> Time
timeOf (Order ( t, u )) =
    t


initFromPair : ( Time, Units ) -> ItemOrder
initFromPair ( t, u ) =
    Order ( t, u )


unitsOf : ItemOrder -> Units
unitsOf (Order ( t, u )) =
    u


init : Time -> Units -> ItemOrder
init t u =
    Order ( t, u )
