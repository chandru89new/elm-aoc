module Day12 exposing (..)

import Dict exposing (Dict)
import List.Extra


type alias Graph =
    Dict String (List String)


addConnection : String -> String -> Graph -> Graph
addConnection a b graph =
    let
        dict1 =
            Dict.update a
                (\s ->
                    case s of
                        Just list ->
                            Just (b :: list)

                        Nothing ->
                            Just [ b ]
                )
                graph

        dict2 =
            Dict.update b
                (\s ->
                    case s of
                        Just list ->
                            Just (a :: list)

                        Nothing ->
                            Just [ a ]
                )
                graph
    in
    Dict.merge Dict.insert (\k v1 v2 -> Dict.update k (\_ -> Just (v1 ++ v2 |> List.Extra.unique))) Dict.insert dict1 dict2 Dict.empty


testInput =
    """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"""


actualInput =
    """
xz-end
CJ-pt
pt-QW
hn-SP
pw-CJ
SP-end
hn-pt
GK-nj
fe-nj
CJ-nj
hn-ZZ
hn-start
hn-fe
ZZ-fe
SP-nj
SP-xz
ZZ-pt
nj-ZZ
start-ZZ
hn-GK
CJ-end
start-fe
CJ-xz"""


inputToGraph input =
    input
        |> String.lines
        |> List.map (String.split "-")
        --[["dc", "end"], ["HN", "start"], ...]
        |> generateGraphFromList Dict.empty
        |> Dict.remove "end"


generateGraphFromList base list =
    let
        updateGraph pairAsList graph_ =
            case pairAsList of
                a :: b :: _ ->
                    addConnection a b graph_

                _ ->
                    graph_
    in
    list
        |> List.foldl updateGraph base



-- traverseMap : String -> String -> AdjacencyMap -> List (List String)
-- getPaths : String -> String -> AdjacencyMap -> List (List String)


findAllPaths : String -> Graph -> List (List String)
findAllPaths start graph =
    let
        go pathsList acc =
            case pathsList of
                [] ->
                    acc

                currPath :: rest ->
                    case currPath of
                        node :: _ ->
                            let
                                adjacents =
                                    Dict.get node graph
                                        |> Maybe.withDefault []
                                        |> List.filter (\s -> not <| List.member s currPath)

                                endOfLine =
                                    List.length adjacents == 0

                                pathsFromAdjacents =
                                    List.map (\a -> a :: currPath) adjacents ++ rest
                            in
                            if endOfLine then
                                go pathsFromAdjacents (currPath :: acc)

                            else
                                go pathsFromAdjacents acc

                        [] ->
                            []
    in
    go [ [ start ] ] [] |> List.map List.reverse |> List.reverse


startsWithLower s =
    s
        |> String.toList
        |> List.head
        |> Maybe.map (\c -> Char.isLower c)
        |> Maybe.withDefault False


findPathsWithEjectFn : (List String -> String -> Bool) -> String -> Graph -> List (List String)
findPathsWithEjectFn ejectFn start graph =
    let
        go pathsList acc =
            case pathsList of
                [] ->
                    acc

                currPath :: rest ->
                    case currPath of
                        node :: _ ->
                            let
                                adjacents =
                                    Dict.get node graph
                                        |> Maybe.withDefault []
                                        |> List.filter (ejectFn currPath)

                                endOfLine =
                                    List.length adjacents == 0 || node == "end"

                                pathsFromAdjacents =
                                    if endOfLine then
                                        rest

                                    else
                                        List.map (\a -> a :: currPath) adjacents ++ rest
                            in
                            if endOfLine then
                                go pathsFromAdjacents (currPath :: acc)

                            else
                                go pathsFromAdjacents acc

                        [] ->
                            []
    in
    go [ [ start ] ] [] |> List.map List.reverse |> List.reverse


sampleGraph =
    Dict.fromList
        [ ( "dc", [ "end", "start", "HN", "LN", "kj" ] )
        , ( "HN", [ "start", "dc", "end", "kj" ] )
        , ( "start", [ "kj", "dc", "HN" ] )
        , ( "LN", [ "dc" ] )
        , ( "kj", [ "sa", "HN", "dc" ] )
        , ( "sa", [ "kj" ] )
        ]


sampleGraph2 =
    Dict.fromList
        [ ( "start", [ "A", "b" ] )
        , ( "A", [ "c", "start", "end", "b" ] )
        , ( "c", [ "A" ] )
        , ( "b", [ "A", "start", "end", "d" ] )
        , ( "d", [ "b" ] )
        , ( "end", [ "A", "b" ] )
        ]


run1 =
    inputToGraph actualInput
        |> findPathsWithEjectFn part1EjectorFn "start"
        |> List.filter (\list -> List.member "end" list)
        |> List.length


part1EjectorFn : List String -> String -> Bool
part1EjectorFn currPath node =
    if not <| startsWithLower node then
        True

    else
        not <| List.member node currPath


part2EjectorFn : List String -> String -> Bool
part2EjectorFn currPath node =
    if not <| startsWithLower node then
        True

    else if node == "start" then
        False

    else if node == "end" then
        let
            f =
                List.filter ((==) node) currPath |> List.length
        in
        f == 0

    else
        not (List.member node currPath)
            || (alreadyHasDoubleSmall currPath |> not)


alreadyHasDoubleSmall path =
    filterLowerCaseOnly path
        -- [b d b]
        |> List.Extra.gatherEquals
        |> List.any (Tuple.second >> (\s -> List.length s > 0))


filterLowerCaseOnly =
    List.filter
        (\s ->
            String.toList s |> List.head |> Maybe.map Char.isLower |> Maybe.withDefault False
        )


run2 =
    inputToGraph actualInput
        |> findPathsWithEjectFn part2EjectorFn "start"
        |> List.filter (\list -> List.member "end" list)
        |> List.length
