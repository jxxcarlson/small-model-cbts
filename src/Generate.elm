module Generate exposing (intList, listWithMean, mean, positiveListWithMean)

import Random exposing (Generator, Seed, initialSeed, int, list, step)


intList_ : Int -> Int -> Int -> Generator (List Int)
intList_ n low high =
    list n (int low high)


{-|

    Generate a list of n randomly distributed integers
    between low and high given the seed

-}
intList : Int -> Int -> Int -> Int -> List Int
intList seed n low high =
    step (intList_ n low high) (initialSeed seed)
        |> Tuple.first


listWithMean : Int -> Int -> Int -> Int -> List Int
listWithMean seed n mean_ spread =
    intList seed n (mean_ - spread) (mean_ + spread)


positiveListWithMean : Int -> Int -> Int -> Int -> List Int
positiveListWithMean seed n mean_ spread =
    listWithMean seed n mean_ spread
        |> List.map (clamp 0 (mean_ + spread))


mean : List Int -> Float
mean numbers =
    let
        n =
            List.length numbers |> toFloat

        s =
            List.sum numbers |> toFloat
    in
    s / n
