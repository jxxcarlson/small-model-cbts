module World exposing (World, init, run, update)

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


run : Config -> World -> World
run config world =
    List.foldl (\n world_ -> update config world_) world (List.range 0 (Future.length world.future - 1))
