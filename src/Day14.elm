module Day14 exposing (..)

import Dict exposing (Dict)


type alias Res =
    { newString : String
    , currTemplate : String
    }


type alias PairLookup =
    Dict String String


inputToPairLookup input =
    input
        |> String.lines
        |> List.map toPairLookup
        |> Dict.fromList


toPairLookup : String -> ( String, String )
toPairLookup str =
    let
        split =
            String.split " -> " str
    in
    case split of
        pair :: replacer :: _ ->
            ( pair, replacer )

        _ ->
            Debug.todo "error parsing a pair string"


foldOnce : Res -> PairLookup -> Res
foldOnce res lookup =
    if String.length res.currTemplate == 1 then
        { newString = res.newString ++ res.currTemplate, currTemplate = "" }

    else
        let
            ( pairToWorkOn, restOfTheString ) =
                splitString res.currTemplate

            newString =
                res.newString ++ doPairInsertion lookup pairToWorkOn
        in
        foldOnce
            { newString = newString
            , currTemplate = restOfTheString
            }
            lookup


splitString : String -> ( String, String )
splitString str =
    let
        pair =
            String.slice 0 2 str

        rest =
            String.slice 1 (String.length str) str
    in
    ( pair, rest )


doPairInsertion : PairLookup -> String -> String
doPairInsertion lookup str =
    Maybe.map
        (\c ->
            String.slice 0 1 str ++ c
        )
        (Dict.get str lookup)
        |> Maybe.withDefault str


foldN : Int -> String -> PairLookup -> String
foldN n string lookup =
    if n == 0 then
        string

    else
        let
            newRes =
                foldOnce { newString = "", currTemplate = string } lookup
        in
        foldN (n - 1) newRes.newString lookup


test =
    foldN 10
        "NNCB"
        (inputToPairLookup testPairs)


counts dict str =
    if str == "" then
        dict

    else
        let
            char =
                String.slice 0 1 str

            newDict =
                Dict.update char
                    (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)
                    dict
        in
        counts newDict (String.slice 1 (String.length str) str)


findMinMax : Dict String Int -> { max : Int, min : Int }
findMinMax dict =
    let
        list =
            Dict.values dict |> List.sort

        reverse =
            List.reverse list

        max =
            List.head reverse |> Maybe.withDefault 0

        min =
            List.head list |> Maybe.withDefault 0
    in
    { max = max, min = min }


run1 =
    foldN 10 "KHSNHFKVVSVPSCVHBHNP" (inputToPairLookup pairs)
        |> counts Dict.empty
        |> findMinMax
        |> (\s -> s.max - s.min)


run2 =
    foldN 40 "NNCB" (inputToPairLookup testPairs)
        |> (counts Dict.empty >> findMinMax)
        |> (\s -> s.max - s.min)


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
