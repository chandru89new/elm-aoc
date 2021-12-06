module Day4 exposing (..)

import Dict exposing (Dict(..))
import Tuple exposing (pair)


type alias RowNumber =
    Int


type alias ColNumber =
    Int


type alias Value =
    Int


type alias Board =
    Dict Coordinate Value


type alias Coordinate =
    ( RowNumber, ColNumber )


winningCombinations : List (List Coordinate)
winningCombinations =
    [ [ pair 0 0, pair 0 1, pair 0 2, pair 0 3, pair 0 4 ]
    , [ pair 1 0, pair 1 1, pair 1 2, pair 1 3, pair 1 4 ]
    , [ pair 2 0, pair 2 1, pair 2 2, pair 2 3, pair 2 4 ]
    , [ pair 3 0, pair 3 1, pair 3 2, pair 3 3, pair 3 4 ]
    , [ pair 4 0, pair 4 1, pair 4 2, pair 4 3, pair 4 4 ]
    , [ pair 0 0, pair 1 0, pair 2 0, pair 3 0, pair 4 0 ]
    , [ pair 0 1, pair 1 1, pair 2 1, pair 3 1, pair 4 1 ]
    , [ pair 0 2, pair 1 2, pair 2 2, pair 3 2, pair 4 2 ]
    , [ pair 0 3, pair 1 3, pair 2 3, pair 3 3, pair 4 3 ]
    , [ pair 0 4, pair 1 4, pair 2 4, pair 3 4, pair 4 4 ]
    ]


boardIntoWinningCombination : Board -> List (List Coordinate) -> List Int -> List (List Bool)
boardIntoWinningCombination board winTemplate nums =
    winTemplate
        |> List.map
            (List.map
                (\cell ->
                    board
                        |> Dict.get cell
                        |> Maybe.map (\x -> List.member x nums)
                        |> Maybe.withDefault False
                )
            )


isBingoBoard : List (List Bool) -> Bool
isBingoBoard list =
    list
        |> List.map (List.all (\a -> a == True))
        |> List.any (\x -> x == True)


exampleBoard =
    """22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19"""


rawInputToBoard : String -> Board
rawInputToBoard rawBoard =
    rawBoard
        |> String.lines
        |> List.map String.trim
        |> List.map (String.replace "  " " ")
        |> List.map (String.split " ")
        |> List.map (List.map (String.toInt >> Maybe.withDefault -1))
        |> List.indexedMap (\row rowVal -> List.indexedMap (\col val -> ( pair row col, val )) rowVal)
        |> List.concat
        |> Dict.fromList


findFirstBingoBoard : List Int -> List Board -> List Int -> ( Board, List Int )
findFirstBingoBoard allNums boards playedNums =
    case allNums of
        [] ->
            ( Dict.empty, playedNums )

        head :: tail ->
            let
                newPlayedNums =
                    playedNums ++ [ head ]

                winningBoards =
                    List.filter
                        (\b -> boardIntoWinningCombination b winningCombinations newPlayedNums |> isBingoBoard)
                        boards
            in
            case winningBoards of
                [] ->
                    findFirstBingoBoard tail boards newPlayedNums

                head_ :: _ ->
                    ( head_, newPlayedNums )


playAllBoards : List Int -> List Board -> List ( Board, List Int ) -> List ( Board, List Int )
playAllBoards allNums boards winnersWithMetaData =
    case boards of
        [] ->
            winnersWithMetaData

        _ :: _ ->
            let
                winner =
                    findFirstBingoBoard allNums boards []

                nonWinners =
                    List.filter (\b -> b /= Tuple.first winner) boards
            in
            playAllBoards allNums nonWinners (winner :: winnersWithMetaData)


boardTemplate =
    [ [ pair 0 0, pair 0 1, pair 0 2, pair 0 3, pair 0 4 ]
    , [ pair 1 0, pair 1 1, pair 1 2, pair 1 3, pair 1 4 ]
    , [ pair 2 0, pair 2 1, pair 2 2, pair 2 3, pair 2 4 ]
    , [ pair 3 0, pair 3 1, pair 3 2, pair 3 3, pair 3 4 ]
    , [ pair 4 0, pair 4 1, pair 4 2, pair 4 3, pair 4 4 ]
    ]


printBoard : Board -> String
printBoard board =
    boardTemplate
        |> List.map
            (\row ->
                List.map
                    (\cell -> Dict.get cell board |> Maybe.map String.fromInt |> Maybe.withDefault "" |> String.padLeft 2 ' ')
                    row
                    |> String.join " "
            )
        |> String.join "\n"


firstBoardToWinAndNums =
    findFirstBingoBoard gameNumbers allBoards []


run1 =
    let
        ( board, nums ) =
            findFirstBingoBoard gameNumbers allBoards []

        sum =
            removeStruckOutNumsFromBoard ( board, nums ) |> sumFromBoardList

        lastNum =
            lastNumberCalled nums
    in
    Maybe.map (\a -> a * sum) lastNum


run2 =
    let
        lastWinnerAndNums =
            playAllBoards gameNumbers allBoards [] |> List.head
    in
    Maybe.andThen
        (\( winningBoard, nums ) ->
            let
                sum =
                    removeStruckOutNumsFromBoard ( winningBoard, nums ) |> sumFromBoardList

                lastNum =
                    lastNumberCalled nums
            in
            Maybe.map (\a -> a * sum) lastNum
        )
        lastWinnerAndNums


removeStruckOutNumsFromBoard : ( Board, List Int ) -> List ( Coordinate, Value )
removeStruckOutNumsFromBoard ( board, nums ) =
    board
        |> Dict.toList
        |> List.filter (\( _, val ) -> not <| List.member val nums)


sumFromBoardList boardAsList =
    boardAsList
        |> List.map Tuple.second
        |> List.foldl (+) 0


lastNumberCalled : List Int -> Maybe Int
lastNumberCalled playedNums =
    playedNums |> List.reverse |> List.head


testGameNumbers =
    [ 7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1 ]


testBoards =
    """22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7""" |> String.split "\n\n" |> List.map rawInputToBoard


gameNumbers =
    [ 79, 9, 13, 43, 53, 51, 40, 47, 56, 27, 0, 14, 33, 60, 61, 36, 72, 48, 83, 42, 10, 86, 41, 75, 16, 80, 15, 93, 95, 45, 68, 96, 84, 11, 85, 63, 18, 31, 35, 74, 71, 91, 39, 88, 55, 6, 21, 12, 58, 29, 69, 37, 44, 98, 89, 78, 17, 64, 59, 76, 54, 30, 65, 82, 28, 50, 32, 77, 66, 24, 1, 70, 92, 23, 8, 49, 38, 73, 94, 26, 22, 34, 97, 25, 87, 19, 57, 7, 2, 3, 46, 67, 90, 62, 20, 5, 52, 99, 81, 4 ]


allBoards =
    """ 7 42 22 92 60
 8 88 99 13 12
16 62 86 24 77
20 57 19 67 46
36 83 54 63 82

 7 86 50 78 16
83 45 67 94 58
21 98 99 85 43
71 19 31 22  4
70 51 34 11 61

 4 95 84 51 36
43 40 37 23 85
14 90  8 59 99
 0 88 68 93 81
25  6 55 19 48

15 39 78  6 13
71  3 81 95 62
22 46 67 72 40
89 69  0 37 41
68 79 58 16 42

63 50 77 34 12
29 42 20 17 47
80 10 30 72 66
 5 89 64 25 21
91 88 45 44 37

78 89 32 26 56
 8 40 54 25 49
36 30 21 23  3
12 58  2 29  7
33 99 15 84 44

96 68 56 49 43
55 22 16 91 32
 2 17 61 12 37
25 72  1 31 88
57 34 42  8 71

18 39 86 94 60
96 85 64 51 28
48 14 23 36 35
 6 84 99 90 81
43 41 74 68 32

 9 58 60  7 61
96 33 67  0 19
77  2 14 99 79
13 36 90 95 29
86 91 49 72 20

 3 79 24 37 97
86 10 77 31 32
48 89 35 73 94
65 21 23 82 36
26 51 69 12 99

66 28 73  6 32
11 30 35 42 76
33 40 25 89 52
46 88 55 50 64
86 71 75 36 80

36 34 35 68 49
61  3 24 84 71
47 42 91 39 80
25 51 38 59 62
90 21 28 52  8

19 93 45 40 55
41 11 79  9 70
16 87 32 22 94
12  4 72 60  0
36 77 78 33 83

43 44  7 39 96
30 75 62 63  8
19 12 40 68 45
50 27  3 52 57
85 67 33 16 36

33 16 66  9  7
93 34 52 31 13
 3 49 94 39 37
76 59 78 51 83
40 47 22 42 73

44 60 52  7 38
36 53 79 11 93
46 65 40 68 58
67 73 99 31 87
22 49 33 59 75

83 61 17 60 86
38 33 96 75 22
19 42 76 55 97
93 94 29 50 88
34 16 91  3 40

92 48 40 69 98
12 46 37 25 78
43 11 34 22 32
 0 18 17 86  1
89 26 65 76 96

66 48 43 99 98
68  2 51 87 38
72 77 47 20 97
36 18 80 10 96
88 53 30 65 91

10  3 65 38 56
40 14 64 45 23
42 88 31 85 17
19 83 46 51  5
35 47 28  0 50

75 53  9  1 29
92 94 41 82 38
39 70 80 11 56
64 28 27 22 60
66 97 48 65 71

91 17 37 49 83
66  1 79 87 60
78 46 32 30 57
50 56 23  6 24
13 89 42 70 77

59 28 58 56 73
22  4 53 91 23
 8 41 36 52 80
30 68 34 70 63
90  3 61 98  1

50 76 99 74 81
57 25 59 69 96
26 15 43 64 44
73 18 61 91 23
87 13 46 90 60

63  1 77 93 47
12 90 56 46  0
57 73 79 87 43
32 13 53 37 14
22  3 23 78 69

49 55 93 57  2
67 12 81 70 79
60 44 94 23 54
48 92 99  1 82
76 36 62 32 98

94 15 97 55 17
39 40 84 92 49
72 45 52 95 96
61 58 88 23 78
80 48 37 35 66

86 88 20 12  7
72 52 95 34 11
 1 47 83 63 18
25 35 76 15 92
96 64 82 54 31

61 83  5 24 36
88 80 48 26 85
 2 42 70 98 45
27  6 65 94 15
71 73  3 47 38

85 49 19 41 53
 4 99 43 93 60
34 28 78 23 50
54 79 35 25 94
27 63 16 51 39

89 49 13  1 32
85 87  8 38 64
14  5 63 16 27
23 76 43 59 94
78 80 83 15 54

26 66 73 74 64
 9 81 62 75 25
46 13 55 43  1
 0  2 10 58 34
76 11 82 42 16

68 93 18 99 84
96 25 44 69 97
24 80 74 27  6
33 14 54 17 28
10 47  2 63 59

12 56 29 63  0
30 94  5 19 18
 9 13 24 72 60
91 46 49 47 51
 8 54 26  7 21

36 16 26 97 56
22 86 58 94 89
66 84 50 82 53
87 29 45 95 33
49 61 46  2 52

87 35 65 27 69
12 98 94 18 26
22 79  1 74 84
 0 72 29 70 19
96 28 95 25 77

79 95  3 91 44
57 61 77 80 29
 6 49 37 62 16
71 73 21 52 48
92 17 32  2 43

29 78  6 94 47
83 63 68 16 56
38 85 92 60 35
81 57 75 79  7
69 22 93 49  4

93 21  2 17 22
76 70  3 80 51
 7 88 14  0 61
18 16 29 86 74
65 47  8 45 46

 1 20 23 79 14
27 76  3 90 85
88 35  7 10 92
67 97 59 41  8
56 57 65 45 81

57 14 41 89 55
47 75 90 23 94
26  3 40 17 97
65 44 12  4 30
16 81 64 79 13

63  3 22  7 10
36 76 14 77 38
48 27 40  9 60
31 56 75 74 78
86 64 71 90 67

52 28  9 19 66
15 86 61  2 89
93  3 44 46 91
11  7  5 32 72
60 10 92 29 88

88 86 59  8 68
10 48 12 61 21
54 97 45 55 11
67  9 22 64  5
 7 34 32 69 44

69 45 14  6  3
16 32 33 26 73
79 30  5  1 72
64  9 60 59 22
23 56 37 41  2

25 65 60 87 39
41 53 24 91 93
43 59 26 78 96
16 33 88 18  7
74 63 34 30 20

38 23 97 73 35
51 31 90 98 80
56 44 60  8  7
71 10 87  0 99
64 30 20 22 18

61 57 31 69 74
94  0 96 90 59
21  3 72 81  4
43 41 58 45  2
62  7 65 71 19

60 20 19 48 11
 2 68 58 91 76
57 12 52 29 13
42 53 38 64 81
26 70 16 32 54

15 93 68 77 49
80 64 45 10 94
30 62  5 66 40
46 51 52 22 56
 7 90 14  6 47

75 87 31 24 11
47 61 14 69 50
33 44 12 26 58
91 10 35  5 29
99 81 16 92 53

50 37 47 13 83
63 96 30 36 86
72 66 93 73 74
98 60  3 84 28
52 14 70 21 55

65 19 32 28 92
 9  8 51  0 98
56 26 53 13 86
 2 70 16 52  4
69 10 97 38 79

34 48 46 66 44
59 19 18 20 13
99 26 62 16  2
91 25 11 84  4
52 31 70 71 14

92  1 49 65 77
85  8 27 87 84
41 73 81 15 58
14 93 33 17 52
35 90 37 38  0

11 46  2 20 31
97 50 12 79 96
89 77 57 61 40
65 75  4 33 17
66 81 47 83 98

34 57 44  0 99
32 25 17 48 90
27 73 63 61 81
50 22  4 28 41
 6 24 70 13 45

96 18 36 16 10
37 11 50 56 88
80 40 75 90 12
19 43 33 61 58
30 59 99 69 98

31 77 98 90 51
34 10 80 73 97
 2 37 33 17  0
59 78 91 87 45
86  7 44 64  1

26 49 66 13 16
95 89 52 88 55
77 60  3 93 73
64 45 98 38 42
34 86  1 71 68

59 71 24 18 99
23 28 88 54 26
90 37  6 76  4
41 64 27 89 67
29 95 82 83 60

 8  0 90 41 61
29 66  2 35 13
12  9  5 36 93
67 94 82 77 37
30 42 32 80 78

53  6 23 57 38
 8 25 76 18 15
19 17 20 48 72
26 54 64  7 40
50 94 82 67 99

93  5 67 10  4
77 80 97 14  2
34  9 61 24 21
63 89 28 76 62
54 29 38 68 69

72 48 66 89 22
63 39 71 59 68
 2 95 94 21 92
 6 28 44 62 15
35 78 80 11 91

82  8 59 66 25
84 87 95 60 12
 9 52 83 28 49
23 34 85 94 96
43 41 39  2 73

81 56 55 29 70
94 96  7 90  2
95 45 28 75 12
48 83 65 22 91
68 98  5 41 73

36 22 45 14 74
35 60 54 15 30
86 49 27 82  4
87  2 52 50 21
39 62 40  1 19

99  7 85 24 65
26 17 36 35  1
 2 62 38 45 48
72 68 32 59 11
28 53 64 21 76

61 63 94 50 55
34 42 39 66 37
22 72 18 89 12
16 23  4  0 41
75 64  3 44  5

87 82 53  5 19
26 54 36  1 38
28 30 48 97 95
34 91 99 23  8
46 35 33 29 66

76 89 94 77 58
24 31  1 40 25
44 71 42 61  8
16 41 28 33 50
 6 85 66 43 51

91 28 70 89 43
 1 76 26 90 45
24  2  6 82 23
77 68 16 51 81
58 86 52 29 18

95  0 25 19 91
10 65 30 72 42
41  8 77 58 23
94 60 34 11 67
24  1 64 78 44

40 76 21 37 15
44 26 80 77 88
25 72 38 34  9
75 81 43 86 68
59 30 87 61 73

 0 63 62 82 93
70 61 14 56  3
54 43 92 78 27
26  7 99 77 73
21 30 44 50 40

 2 60 45 17 73
75 67 68 20 18
16 30 24 37 12
79 50  8 65 19
85 95 54 90 47

69 68 54 66 17
39 19 20 33 44
12 27 50 60 36
53 81  8  7 87
82 97 18  4 74

58 63  8 42 28
70 95 39 54 61
30 56 79 37 82
15 32 83 27 45
52 13 90 97 62

11 50 56 66 84
96 94 57 17 49
68 58 90 34 59
81 36 91  8 45
62 35  6 93 48

82 89 54 87 80
94  6 45 53 62
31 34 58 85 77
24 25 91 99 26
41  0 59 37 23

93 41 53 31 87
 7 22 39 86 73
71 34 60 57  6
52 64 48 99 90
66 76 62 45 40

 5 84 85 67 26
11  1  0 95 21
48 59 43 94 62
22 74 40 49 89
51 20 90 78 96

 0 45 43 79 25
41 10 95 86 80
 4 60 82 33 75
44 46 38 17 76
22 58 27 73 66

54 50  7 92 79
11 43 38 94  5
63 80 33 58  4
12 91 28 70 97
26 99 41 52 90

23 26 95  8 17
73 77 61 89 82
78 80 64 19 96
81 92 47 44 59
54 24 63 74 32

86 85 37 80 45
47 44 92 29 49
67 48 95 51 88
36  8 56 16 30
 0 97 84 24 13

81 61 42 87 92
30 75 17 67  2
83 44 96 52  1
37 78 31 15 19
40  9 72  7 28

10 85 17 38 22
46 35 90 12 27
76 42  7  2 30
55 57 60  9 49
79 73 97  1 21

52 36 11 82 91
22  7 46 21 12
62 42 66 68 10
31 18 76 20 84
28 79 61 39 86

73 99 34 54 45
43 28 18 76 40
57 58 63  9 11
89 65  2 12 90
38 97 49 15 27

28 84 24 17 49
33 69 75 53 92
81 48 89 19 34
59  1 18 72 79
 6 22  2 86 85

72 78 30 40 19
54 16 25 81 28
41 99  7 79 14
83 76 29  8 91
 5 60 11 51 37

77 78 34 59 29
62 69 54  8 97
80 53 25 66 85
81 90 31 51 52
63 41 57 68 18

43 62 11 41  7
37 44 34 10 51
67 36 61 77 70
59  1 25 42 88
29 71 60 15 24

30 65 57 35 84
34 33 72 73 28
38 51  4 52 14
58 59 85 87 39
88 81 11 93 71

19  5 23 71 75
70  9 57 69 14
49 29 22 28 10
42 48 63 73  6
79 18  4 39 88

16 27 31 88 86
29 40 65 68 39
15 95 93 69 22
66 48 18 84 11
 7 51 92 96 99

 0 69 51 12 82
 4 81 62  2 49
27 66 95 83 70
94 97 99 63 19
87 75 77 73 44

82 83 75 95 53
46 47 31 14 64
71 70 11 51 87
 7 16 63 38 29
89 13 33 41  0""" |> String.split "\n\n" |> List.map rawInputToBoard
