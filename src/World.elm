module World exposing (World, init, update)

import OrderSequence exposing (Future)
import State exposing (State)


type alias World =
    { state : State
    , future : Future
    }


init : State -> Future -> World
init state future =
    { state = state, future = future }


update : World -> World
update world =
    case OrderSequence.head world.future of
        Nothing ->
            world

        Just itemOrder ->
            let
                newState =
                    State.update itemOrder world.state

                future =
                    OrderSequence.tail world.future
            in
            { state = newState, future = future }
