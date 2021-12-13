module Day11 exposing (..)

import Day5 exposing (Point)
import Dict exposing (Dict)
import List.Extra
import Tuple exposing (pair)



{-

   8769
   7653
   1218
   1675

   - step:
   9970
   8764
   2329
   2786
   - flashed: [(0,3)]
   - flash-effect-processor:
   9980
   8775
   2329
   2786
   - flash effect processing done.
   - no new flashes occured so flashed: []

   - step:
   0091
   9886
   3430
   3897
   - flashed: [0,0; 0,1; 2,3]
   - flash-effect-processor:
   0001
   0096
   3430
   3897
   -flashed: [1,0;0,1;2,3;0,2;1,1]


-}


type alias Grid =
    Dict Point Int


type alias Point =
    ( Int, Int )


flashEffect : Grid -> List Point -> List Point -> Int -> ( Grid, List Point, Int )
flashEffect grid justFlashed allFlashedInStep countOfNewFlashes =
    case justFlashed of
        [] ->
            ( grid, allFlashedInStep, countOfNewFlashes )

        head :: tail ->
            let
                ( flashedGrid, newFlashes ) =
                    flashOnNeighbors head allFlashedInStep grid
            in
            flashEffect flashedGrid (tail ++ newFlashes) (allFlashedInStep ++ newFlashes) (countOfNewFlashes + List.length newFlashes)


flashOnNeighbors : Point -> List Point -> Grid -> ( Grid, List Point )
flashOnNeighbors point alreadyFlashed grid =
    let
        neighbors : Grid
        neighbors =
            getNeighbors point grid |> filterNewlyFlashed alreadyFlashed

        inc : Grid
        inc =
            Dict.map (\_ v -> incrementEnergy v) neighbors

        newlyFlashed : List Point
        newlyFlashed =
            filterJustFlashed inc

        newGrid : Grid
        newGrid =
            Dict.union inc grid
    in
    ( newGrid, newlyFlashed )


incrementEnergy int =
    if int == 9 then
        0

    else
        int + 1


getNeighbors : Point -> Grid -> Grid
getNeighbors ( x, y ) grid =
    [ pair (x - 1) (y - 1)
    , pair (x - 1) y
    , pair (x - 1) (y + 1)
    , pair x (y - 1)
    , pair x (y + 1)
    , pair (x + 1) (y - 1)
    , pair (x + 1) y
    , pair (x + 1) (y + 1)
    ]
        |> List.filterMap (\p -> Maybe.map (\v -> ( p, v )) (Dict.get p grid))
        |> Dict.fromList


filterNewlyFlashed : List Point -> Grid -> Grid
filterNewlyFlashed points grid =
    Dict.filter (\k _ -> not <| List.member k points) grid


filterJustFlashed : Grid -> List Point
filterJustFlashed grid =
    Dict.filter (\_ v -> v == 0) grid
        |> Dict.toList
        |> List.map Tuple.first


sampleGrid =
    [ ( ( 0, 0 ), 9 ), ( ( 0, 1 ), 9 ), ( ( 0, 2 ), 2 ), ( ( 1, 0 ), 2 ), ( ( 1, 1 ), 8 ), ( ( 1, 2 ), 2 ), ( ( 2, 0 ), 2 ), ( ( 2, 1 ), 2 ), ( ( 2, 2 ), 2 ) ]
        |> Dict.fromList


gridToPrint : Grid -> String
gridToPrint grid =
    grid
        |> Dict.toList
        |> List.map (Tuple.second >> String.fromInt)
        |> List.Extra.groupsOf 10
        |> List.map (String.join "")
        |> String.join "\n"


step : Grid -> ( Grid, List Point, Int )
step grid =
    let
        incGrid =
            incAll grid

        newlyFlashed =
            flashed incGrid

        ( newGrid, allFlashes, count ) =
            flashEffect incGrid newlyFlashed newlyFlashed (List.length newlyFlashed)

        incAll : Grid -> Grid
        incAll =
            Dict.map (\_ v -> incrementEnergy v)

        flashed : Grid -> List Point
        flashed grid_ =
            Dict.toList grid_
                |> List.filter (\( _, v ) -> v == 0)
                |> List.map Tuple.first
    in
    ( newGrid, allFlashes, count )


doNSteps : Int -> Int -> Grid -> ( Int, Grid )
doNSteps n count grid =
    if n == 0 then
        ( count, grid )

    else
        let
            ( newGrid, _, newCount ) =
                step grid
        in
        doNSteps (n - 1) (count + newCount) newGrid


run1 =
    inputToGrid actualInput
        |> doNSteps 100 0
        |> Tuple.first


findFirstAllFlash : Grid -> Int
findFirstAllFlash grid =
    let
        go : Grid -> Int -> Int
        go grid_ count =
            let
                ( newGrid, _, _ ) =
                    step grid_

                asList =
                    Dict.toList newGrid

                allFlashed =
                    asList
                        |> List.filter (\( _, v ) -> v == 0)
                        |> (\s -> List.length s == List.length asList)
            in
            if allFlashed then
                count

            else
                go newGrid (count + 1)
    in
    go grid 0


run2 =
    findFirstAllFlash (inputToGrid actualInput)


inputToGrid : String -> Grid
inputToGrid str =
    str
        |> String.lines
        |> List.map (String.split "")
        |> List.indexedMap
            (\rId ->
                List.indexedMap (\cId c -> pair ( rId, cId ) (unsafeStrToInt c))
            )
        |> List.concat
        |> Dict.fromList


unsafeStrToInt : String -> Int
unsafeStrToInt str =
    case String.toInt str of
        Just x ->
            x

        Nothing ->
            Debug.todo "couldnt convert string to int"


testInput =
    """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""


actualInput =
    """2344671212
6611742681
5575575573
3167848536
1353827311
4416463266
2624761615
1786561263
3622643215
4143284653
"""
