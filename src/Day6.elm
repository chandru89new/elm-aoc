module Day6 exposing (..)


testInput =
    [ 3, 4, 3, 1, 2 ]


type alias Tally =
    { eight : Int
    , seven : Int
    , six : Int
    , five : Int
    , four : Int
    , three : Int
    , two : Int
    , one : Int
    , zero : Int
    }


initTally : Tally
initTally =
    Tally 0 0 0 0 0 0 0 0 0


inputToTally list =
    List.foldl
        (\int acc ->
            { eight =
                acc.eight
                    + (if int == 8 then
                        1

                       else
                        0
                      )
            , seven =
                acc.seven
                    + (if int == 7 then
                        1

                       else
                        0
                      )
            , six =
                acc.six
                    + (if int == 6 then
                        1

                       else
                        0
                      )
            , five =
                acc.five
                    + (if int == 5 then
                        1

                       else
                        0
                      )
            , four =
                acc.four
                    + (if int == 4 then
                        1

                       else
                        0
                      )
            , three =
                acc.three
                    + (if int == 3 then
                        1

                       else
                        0
                      )
            , two =
                acc.two
                    + (if int == 2 then
                        1

                       else
                        0
                      )
            , one =
                acc.one
                    + (if int == 1 then
                        1

                       else
                        0
                      )
            , zero =
                acc.zero
                    + (if int == 0 then
                        1

                       else
                        0
                      )
            }
        )
        initTally
        list


getNewGen : List Int -> List Int
getNewGen list =
    list
        |> List.filter (\int -> int == 0)
        |> List.length
        |> (\times -> List.repeat times 8)


resetGen : List Int -> List Int
resetGen list =
    list
        |> List.map
            (\int ->
                if int == 0 then
                    6

                else
                    int - 1
            )


runOneDay : List Int -> List Int
runOneDay list =
    resetGen list ++ getNewGen list


runDays : Int -> List Int -> List Int
runDays days startingData =
    if days == 0 then
        startingData

    else
        runDays (days - 1) (resetGen startingData ++ getNewGen startingData)


input =
    [ 2, 4, 1, 5, 1, 3, 1, 1, 5, 2, 2, 5, 4, 2, 1, 2, 5, 3, 2, 4, 1, 3, 5, 3, 1, 3, 1, 3, 5, 4, 1, 1, 1, 1, 5, 1, 2, 5, 5, 5, 2, 3, 4, 1, 1, 1, 2, 1, 4, 1, 3, 2, 1, 4, 3, 1, 4, 1, 5, 4, 5, 1, 4, 1, 2, 2, 3, 1, 1, 1, 2, 5, 1, 1, 1, 2, 1, 1, 2, 2, 1, 4, 3, 3, 1, 1, 1, 2, 1, 2, 5, 4, 1, 4, 3, 1, 5, 5, 1, 3, 1, 5, 1, 5, 2, 4, 5, 1, 2, 1, 1, 5, 4, 1, 1, 4, 5, 3, 1, 4, 5, 1, 3, 2, 2, 1, 1, 1, 4, 5, 2, 2, 5, 1, 4, 5, 2, 1, 1, 5, 3, 1, 1, 1, 3, 1, 2, 3, 3, 1, 4, 3, 1, 2, 3, 1, 4, 2, 1, 2, 5, 4, 2, 5, 4, 1, 1, 2, 1, 2, 4, 3, 3, 1, 1, 5, 1, 1, 1, 1, 1, 3, 1, 4, 1, 4, 1, 2, 3, 5, 1, 2, 5, 4, 5, 4, 1, 3, 1, 4, 3, 1, 2, 2, 2, 1, 5, 1, 1, 1, 3, 2, 1, 3, 5, 2, 1, 1, 4, 4, 3, 5, 3, 5, 1, 4, 3, 1, 3, 5, 1, 3, 4, 1, 2, 5, 2, 1, 5, 4, 3, 4, 1, 3, 3, 5, 1, 1, 3, 5, 3, 3, 4, 3, 5, 5, 1, 4, 1, 1, 3, 5, 5, 1, 5, 4, 4, 1, 3, 1, 1, 1, 1, 3, 2, 1, 2, 3, 1, 5, 1, 1, 1, 4, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 5, 3 ]


getNewGenN : Tally -> Int
getNewGenN tally =
    tally.six


resetTally : Tally -> Tally
resetTally tally =
    { tally | zero = 0, six = tally.zero }


runOneDayN : Tally -> Tally
runOneDayN tally =
    { tally | zero = tally.one, one = tally.two, two = tally.three, three = tally.four, four = tally.five, five = tally.six, six = tally.seven + tally.zero, seven = tally.eight, eight = tally.zero }


runDaysN : Int -> Tally -> Tally
runDaysN days tally =
    if days == 0 then
        tally

    else
        runDaysN (days - 1) (runOneDayN tally)


count : Tally -> Int
count tally =
    tally.zero + tally.one + tally.two + tally.three + tally.four + tally.five + tally.six + tally.seven + tally.eight


run1_ =
    runDays 80 input |> List.length


run1 =
    runDaysN 80 (inputToTally input) |> count


run2_ =
    {- run2_  will not work. node runs out of memory -}
    runDays 256 input |> List.length


run2 =
    runDaysN 256 (inputToTally input) |> count
