module Day14Attempt2 exposing (..)

import Day14 exposing (Res)
import Dict exposing (Dict)
import List exposing (all)


testPairs =
    """CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"""


testTemplate =
    """NNCB"""


type alias PairLookup =
    Dict String (List String)


type alias Counts =
    Dict String Int


makeLookupDict : String -> PairLookup
makeLookupDict str =
    str
        |> String.lines
        |> List.map toPairLookup
        |> Dict.fromList


toPairLookup : String -> ( String, List String )
toPairLookup str =
    let
        ( pair, replacer ) =
            ( String.slice 0 2 str, String.slice -1 10 str )

        ( char1, char2 ) =
            Tuple.pair (String.slice 0 1 str) (String.slice 1 2 str)
    in
    ( pair, [ char1 ++ replacer, replacer ++ char2 ] )


fold : PairLookup -> Counts -> Counts
fold lookup dict =
    let
        keys =
            Dict.keys dict

        resultList =
            List.concatMap (replicateKey lookup dict) keys

        res =
            List.foldl merger resetDict resultList

        resetDict =
            Dict.union
                (keys |> List.map (\s -> Tuple.pair s 0) |> Dict.fromList)
                dict

        merger pair =
            mergeStrategy (Dict.insert (Tuple.first pair) (Tuple.second pair) Dict.empty)
    in
    res


replicateKey : PairLookup -> Counts -> String -> List ( String, Int )
replicateKey lookup dict key =
    let
        val =
            Dict.get key dict |> Maybe.withDefault 1

        pairs_ =
            Dict.get key lookup |> Maybe.withDefault []

        toTuple str =
            ( str, val )
    in
    List.map toTuple pairs_


allChars =
    List.map (Char.fromCode >> String.fromChar) (List.range 65 90)


getCountOfChar : Counts -> String -> Int
getCountOfChar dict char =
    let
        charInLeftOfKey char_ key =
            String.slice 0 1 key == char_

        charInRightOfKey char_ key =
            String.slice 1 2 key == char_

        dictWithLeftKey =
            Dict.filter (\k v -> charInLeftOfKey char k && v > 0) dict

        dictWithRightKey =
            Dict.filter (\k v -> charInRightOfKey char k && v > 0) dict

        count =
            Dict.foldl (\_ v s -> s + v)

        l =
            count 0 dictWithLeftKey

        r =
            count 0 dictWithRightKey
    in
    if l > r then
        l

    else
        r


foldN : Int -> PairLookup -> Counts -> Counts
foldN n l c =
    if n == 0 then
        c

    else
        foldN (n - 1) l (fold l c)


mergeStrategy dict1 dict2 =
    Dict.merge
        Dict.insert
        (\k v1 v2 -> Dict.insert k (v1 + v2))
        Dict.insert
        dict1
        dict2
        Dict.empty


log =
    Debug.log "log"


templateToDict str =
    str
        |> toPairs []
        |> List.map (\s -> Tuple.pair s 1)
        |> Dict.fromList


toPairs : List String -> String -> List String
toPairs res str =
    if String.length str < 2 then
        res

    else
        let
            pair =
                String.slice 0 2 str

            rest =
                String.slice 1 (String.length str) str
        in
        toPairs ((pair :: List.reverse res) |> List.reverse) rest


countOfAllChars dict =
    allChars
        |> List.map (getCountOfChar dict)


run1 =
    foldN 10 (makeLookupDict pairs) (templateToDict template)
        |> countOfAllChars
        |> List.filter (\v -> v > 0)
        |> (\s -> Maybe.map2 (-) (List.maximum s) (List.minimum s))


run2 =
    foldN 40 (makeLookupDict pairs) (templateToDict template)
        |> countOfAllChars
        |> List.filter (\v -> v > 0)
        |> (\s -> Maybe.map2 (-) (List.maximum s) (List.minimum s))


template =
    """KHSNHFKVVSVPSCVHBHNP"""


pairs =
    """FV -> H
SB -> P
NV -> S
BS -> K
KB -> V
HB -> H
NB -> N
VB -> P
CN -> C
CF -> N
OF -> P
FO -> K
OC -> F
BN -> V
PO -> O
OS -> B
KH -> N
BB -> C
PV -> K
ON -> K
NF -> H
BV -> K
SN -> N
PB -> S
PK -> F
PF -> S
BP -> K
SP -> K
NN -> K
FP -> N
NK -> N
SF -> P
HS -> C
OH -> C
FS -> H
VH -> N
CO -> P
VP -> H
FF -> N
KP -> B
BH -> B
PP -> F
SS -> P
CV -> S
HO -> P
PN -> K
SO -> O
NO -> O
NH -> V
HH -> F
KK -> C
VO -> B
KS -> B
SV -> O
OP -> S
VK -> H
KF -> O
CP -> H
SH -> H
NC -> S
KC -> O
CK -> H
CH -> B
KO -> O
OV -> P
VF -> V
HN -> P
FH -> P
BC -> V
HV -> N
BO -> V
PH -> P
NP -> F
FN -> F
FK -> P
SC -> C
KN -> S
NS -> S
OK -> S
HK -> O
PC -> O
BK -> O
OO -> P
BF -> N
SK -> V
VS -> B
HP -> H
VC -> V
KV -> P
FC -> H
HC -> O
HF -> S
CB -> H
CC -> B
PS -> C
OB -> B
CS -> S
VV -> S
VN -> H
FB -> N"""
