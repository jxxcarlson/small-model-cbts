module Message exposing
    ( Messages
    , init
    , push
    , stringVal
    , takeFront
    )

import BoundedDeque exposing (BoundedDeque)


type Messages
    = Messages (BoundedDeque String)


init : Int -> Messages
init capacity =
    Messages (BoundedDeque.empty capacity)


push : String -> Messages -> Messages
push str (Messages data) =
    Messages (BoundedDeque.pushFront str data)


takeFront : Int -> Messages -> List String
takeFront k (Messages data) =
    BoundedDeque.takeFront k data


stringVal : Int -> Messages -> String
stringVal k (Messages data) =
    BoundedDeque.takeFront k data
        |> String.join " || "
