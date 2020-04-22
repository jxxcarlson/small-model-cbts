module World exposing (World, init, update)

import Config exposing (Config)
import Future exposing (Future)
import State exposing (State)


type alias World =
    { state : State
    , future : Future
    }


init : State -> Future -> World
init state future =
    { state = state, future = future }


update : Config -> World -> World
update config world =
    case Future.head world.future of
        Nothing ->
            world

        Just itemOrder ->
            let
                newState =
                    State.update config itemOrder world.state

                future =
                    Future.tail world.future
            in
            { state = newState, future = future }
