module Day5More exposing (..)

import Day5 exposing (range)
import Dict exposing (Dict)
import List.Extra
import Maybe.Extra


testInput =
    """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""


whichCoordChanged : Coord -> Coord -> Change
whichCoordChanged ( x1, y1 ) ( x2, y2 ) =
    if x1 == x2 && y1 == y2 then
        Neither

    else if x1 == x2 then
        Y

    else if y1 == y2 then
        X

    else
        Both


toPoints : Coord -> Coord -> List Coord
toPoints ( x1, y1 ) ( x2, y2 ) =
    case whichCoordChanged ( x1, y1 ) ( x2, y2 ) of
        X ->
            range x1 x2 |> List.map (\x -> ( x, y1 ))

        Y ->
            range y1 y2 |> List.map (\y -> ( x1, y ))

        Neither ->
            []

        Both ->
            [ range x1 x2, range y1 y2 ] |> List.Extra.transpose |> List.map toPoint


toPoint : List Int -> Coord
toPoint list =
    case list of
        h :: t :: [] ->
            ( h, t )

        _ ->
            ( 0, 0 )


strToLine : String -> Maybe Line
strToLine str =
    str
        |> String.split " -> "
        |> List.map
            (\pair ->
                pair
                    |> String.split ","
                    |> List.map String.toInt
                    |> Maybe.Extra.combine
                    |> Maybe.map toPoint
            )
        |> Maybe.Extra.combine
        |> Maybe.andThen listToLine


listToLine : List Coord -> Maybe Line
listToLine list =
    case list of
        s :: e :: [] ->
            Just ( s, e )

        _ ->
            Nothing


listOfLines : String -> Maybe (List Line)
listOfLines str =
    str
        |> String.lines
        |> List.map strToLine
        |> Maybe.Extra.combine


intersectionData2 inp =
    inp
        |> listOfLines
        |> Maybe.map (\lines -> List.concatMap toCoords lines |> toDict)


intersectionData1 inp =
    inp
        |> listOfLines
        |> Maybe.map (List.filter filterHorizVert)
        |> Maybe.map (\lines -> List.concatMap toCoords lines |> toDict)


run1 =
    intersectionData1 actualInput |> Maybe.withDefault Dict.empty >> Dict.values |> List.filter (\int -> int > 1) |> List.length


run2 =
    intersectionData2 actualInput |> Maybe.withDefault Dict.empty >> Dict.values |> List.filter (\int -> int > 1) |> List.length


filterHorizVert : Line -> Bool
filterHorizVert ( c1, c2 ) =
    case whichCoordChanged c1 c2 of
        Both ->
            False

        _ ->
            True


toCoords : Line -> List Coord
toCoords line =
    toPoints (Tuple.first line) (Tuple.second line)


toDict : List Coord -> Dict Coord Int
toDict list =
    let
        go : List Coord -> Dict Coord Int -> Dict Coord Int
        go lst acc =
            case lst of
                [] ->
                    acc

                h :: t ->
                    go t (Dict.update h incIfExists acc)
    in
    go list Dict.empty


incIfExists : Maybe Int -> Maybe Int
incIfExists maybeInt =
    case maybeInt of
        Just x ->
            Just (x + 1)

        Nothing ->
            Just 1


type alias Coord =
    ( Int, Int )


type alias Line =
    ( Coord, Coord )


type Change
    = X
    | Y
    | Both
    | Neither


actualInput =
    """284,294 -> 733,743
625,347 -> 653,375
561,848 -> 561,181
836,102 -> 836,339
946,941 -> 22,17
18,954 -> 956,16
370,142 -> 370,700
990,966 -> 677,966
366,603 -> 366,465
728,942 -> 57,271
615,493 -> 847,493
584,281 -> 301,281
125,356 -> 301,180
941,569 -> 555,183
151,116 -> 509,116
41,18 -> 841,818
627,670 -> 627,630
965,472 -> 965,100
93,404 -> 330,641
475,963 -> 475,514
389,389 -> 389,326
842,565 -> 842,576
454,700 -> 650,700
73,810 -> 73,319
450,212 -> 450,284
316,392 -> 316,697
915,592 -> 578,592
622,485 -> 434,485
109,853 -> 952,10
305,73 -> 305,222
27,489 -> 157,489
191,979 -> 867,979
527,329 -> 527,292
301,645 -> 301,162
639,730 -> 176,730
46,964 -> 46,458
727,422 -> 435,714
28,552 -> 404,552
33,108 -> 33,21
227,249 -> 327,249
414,903 -> 784,903
69,422 -> 888,422
422,924 -> 103,605
793,353 -> 450,10
714,682 -> 714,972
201,745 -> 410,745
408,713 -> 408,847
174,842 -> 818,198
863,353 -> 775,353
199,780 -> 670,780
877,947 -> 340,410
163,202 -> 163,91
955,919 -> 955,585
836,271 -> 533,271
258,366 -> 728,836
582,749 -> 582,12
80,40 -> 80,704
287,213 -> 287,635
390,546 -> 390,194
837,511 -> 538,810
473,281 -> 902,281
851,865 -> 731,745
918,59 -> 445,532
796,215 -> 796,248
875,111 -> 604,111
660,805 -> 538,805
507,850 -> 145,850
585,861 -> 585,52
426,74 -> 700,348
206,405 -> 529,405
418,333 -> 418,17
368,457 -> 33,792
186,81 -> 957,852
505,283 -> 113,283
20,878 -> 462,878
750,237 -> 69,918
15,280 -> 358,623
798,981 -> 500,683
965,970 -> 22,970
950,970 -> 148,970
660,392 -> 660,884
862,405 -> 862,527
801,283 -> 801,361
71,837 -> 136,837
651,438 -> 945,144
524,607 -> 614,517
348,955 -> 138,955
957,164 -> 404,717
531,581 -> 454,504
710,185 -> 710,271
822,86 -> 822,966
745,233 -> 490,488
350,823 -> 663,823
824,67 -> 447,444
846,667 -> 796,617
666,24 -> 666,906
640,39 -> 640,145
654,481 -> 985,481
581,894 -> 416,729
443,11 -> 697,11
318,627 -> 799,146
113,78 -> 891,856
181,149 -> 179,151
451,74 -> 451,262
458,726 -> 314,726
218,662 -> 533,662
965,108 -> 527,108
782,481 -> 896,367
557,927 -> 557,938
506,242 -> 941,677
948,778 -> 948,629
567,816 -> 567,956
323,773 -> 323,364
864,980 -> 864,12
611,699 -> 611,886
613,392 -> 901,104
528,905 -> 156,905
632,206 -> 798,40
338,237 -> 919,818
256,889 -> 11,644
835,52 -> 55,832
464,144 -> 322,144
254,747 -> 254,509
866,892 -> 866,916
827,946 -> 30,149
899,84 -> 177,806
134,634 -> 357,634
781,492 -> 244,492
817,762 -> 817,976
818,749 -> 818,860
262,480 -> 263,480
409,576 -> 409,698
242,151 -> 981,890
149,519 -> 149,557
42,990 -> 42,930
687,974 -> 50,337
758,382 -> 465,382
760,861 -> 760,934
17,835 -> 17,915
645,923 -> 645,648
702,116 -> 72,746
153,162 -> 955,964
185,101 -> 918,834
554,179 -> 554,353
879,673 -> 879,949
368,13 -> 368,512
582,105 -> 591,114
146,291 -> 600,745
609,538 -> 930,538
320,604 -> 320,146
566,698 -> 443,575
167,708 -> 844,31
712,630 -> 712,421
912,930 -> 64,82
980,931 -> 87,38
23,893 -> 888,28
640,435 -> 676,435
701,516 -> 190,516
684,145 -> 62,767
127,471 -> 91,435
685,197 -> 78,197
103,493 -> 103,522
309,986 -> 309,850
938,270 -> 938,300
295,72 -> 354,72
948,889 -> 948,455
254,733 -> 254,175
95,329 -> 942,329
19,672 -> 19,445
206,807 -> 206,934
886,961 -> 886,690
117,386 -> 117,292
199,59 -> 668,528
299,263 -> 299,878
28,295 -> 638,905
10,140 -> 276,406
279,526 -> 921,526
485,128 -> 856,499
418,398 -> 186,398
296,577 -> 296,521
514,261 -> 10,765
691,673 -> 776,758
131,430 -> 152,430
858,85 -> 62,85
394,846 -> 270,970
827,913 -> 827,376
634,669 -> 910,669
12,53 -> 945,986
782,467 -> 782,421
159,832 -> 109,832
793,807 -> 79,93
120,584 -> 356,584
645,16 -> 645,355
526,685 -> 217,376
296,305 -> 296,929
954,144 -> 954,839
748,88 -> 103,733
523,804 -> 473,754
524,316 -> 524,756
696,183 -> 912,183
288,564 -> 55,797
568,103 -> 568,348
468,626 -> 682,412
163,163 -> 961,961
762,824 -> 27,89
623,625 -> 32,34
865,343 -> 490,718
259,458 -> 259,33
944,660 -> 944,176
781,804 -> 826,759
15,702 -> 15,553
403,310 -> 918,825
438,734 -> 835,734
825,13 -> 825,245
129,611 -> 370,611
49,939 -> 172,939
687,906 -> 687,532
629,482 -> 273,126
727,218 -> 424,218
447,451 -> 233,451
142,779 -> 813,779
527,27 -> 527,804
482,55 -> 482,200
39,264 -> 806,264
884,636 -> 458,636
467,121 -> 199,389
856,925 -> 856,666
666,359 -> 378,359
11,946 -> 705,946
491,281 -> 940,730
86,112 -> 918,944
974,807 -> 974,707
445,67 -> 914,536
953,394 -> 953,822
468,398 -> 157,87
231,620 -> 231,646
979,869 -> 979,911
450,330 -> 450,79
675,659 -> 617,659
66,181 -> 66,723
181,406 -> 181,192
908,334 -> 908,526
254,891 -> 282,891
777,791 -> 127,141
469,58 -> 694,58
954,957 -> 566,569
957,957 -> 123,123
741,359 -> 741,986
763,526 -> 763,101
857,427 -> 600,170
527,756 -> 490,719
625,249 -> 397,249
798,702 -> 712,702
868,75 -> 868,853
332,296 -> 332,629
211,829 -> 100,940
12,139 -> 12,218
655,978 -> 655,242
99,852 -> 855,96
486,267 -> 486,855
474,90 -> 474,244
948,491 -> 186,491
896,59 -> 278,677
295,732 -> 629,732
860,936 -> 860,556
143,790 -> 143,26
371,847 -> 395,847
739,301 -> 739,44
384,716 -> 748,716
848,423 -> 848,923
855,23 -> 218,660
381,805 -> 381,438
451,610 -> 91,610
906,957 -> 191,957
118,675 -> 169,675
836,818 -> 95,818
368,945 -> 825,488
165,299 -> 899,299
392,327 -> 926,861
663,16 -> 131,548
630,302 -> 888,302
206,869 -> 206,331
979,413 -> 979,204
894,860 -> 62,28
444,897 -> 962,379
550,158 -> 550,885
845,736 -> 811,736
846,857 -> 12,857
981,730 -> 981,154
694,835 -> 88,835
21,101 -> 21,385
19,960 -> 964,15
283,721 -> 450,721
59,136 -> 758,835
287,313 -> 719,313
471,252 -> 849,630
682,189 -> 168,189
10,921 -> 774,157
884,598 -> 884,540
207,615 -> 207,443
627,408 -> 67,408
285,36 -> 285,792
116,585 -> 254,585
183,86 -> 183,702
220,138 -> 868,138
833,68 -> 286,615
367,534 -> 766,534
907,514 -> 621,228
133,593 -> 133,581
164,727 -> 768,123
566,227 -> 566,555
983,988 -> 105,110
620,177 -> 620,821
612,413 -> 612,176
168,889 -> 168,210
871,487 -> 559,175
399,870 -> 761,870
236,976 -> 582,630
699,216 -> 699,887
153,745 -> 790,745
444,749 -> 444,257
808,165 -> 939,165
546,525 -> 95,976
583,179 -> 373,389
235,816 -> 840,816
744,89 -> 832,89
425,317 -> 465,357
267,235 -> 114,82
887,59 -> 572,374
808,237 -> 808,626
431,352 -> 400,383
815,376 -> 815,905
249,218 -> 989,958
120,435 -> 357,198
807,551 -> 490,234
910,524 -> 910,725
802,304 -> 447,659
789,228 -> 678,339
229,322 -> 52,322
658,393 -> 506,393
378,438 -> 378,569
163,981 -> 473,671
537,984 -> 935,586
58,945 -> 966,37
132,696 -> 565,263
136,813 -> 136,284
606,656 -> 298,348
533,572 -> 673,712
872,912 -> 301,341
16,287 -> 16,613
571,541 -> 980,950
117,495 -> 35,495
85,79 -> 682,676
425,431 -> 117,739
982,984 -> 10,12
28,75 -> 431,478
259,529 -> 259,436
762,267 -> 170,859
323,135 -> 929,741
81,238 -> 561,718
128,213 -> 876,961
649,466 -> 649,540
715,863 -> 119,863
830,624 -> 794,660
123,968 -> 977,114
489,466 -> 489,811
27,10 -> 980,963
255,732 -> 255,484
574,829 -> 431,829
548,743 -> 22,217
903,297 -> 903,763
684,774 -> 64,154
260,823 -> 683,823
422,211 -> 422,826
10,196 -> 988,196
108,802 -> 15,802
104,70 -> 104,452
885,59 -> 885,36
68,854 -> 68,774
731,935 -> 731,718
657,986 -> 617,986
732,292 -> 732,32
841,56 -> 841,83
74,108 -> 862,896
654,895 -> 323,895
374,952 -> 374,217
90,723 -> 750,63
246,89 -> 911,754
453,301 -> 755,301
983,988 -> 23,28
81,705 -> 133,757
752,743 -> 752,397
53,243 -> 449,639
451,811 -> 451,187
26,672 -> 26,699
254,861 -> 943,861
643,740 -> 643,966
486,655 -> 149,318
375,146 -> 375,973
76,293 -> 103,293
246,398 -> 246,248
324,392 -> 595,121
130,577 -> 131,577
380,623 -> 549,454
224,181 -> 985,942
310,223 -> 310,594
23,982 -> 23,738
19,858 -> 832,858
726,531 -> 726,578
730,433 -> 196,433
606,599 -> 242,599
444,832 -> 444,238
198,870 -> 47,870
944,473 -> 795,473
737,386 -> 178,945
328,902 -> 328,644
422,851 -> 567,851
674,781 -> 215,781
920,757 -> 302,757
225,932 -> 640,517
359,337 -> 791,337
935,430 -> 935,262
772,850 -> 280,358
175,829 -> 175,451
938,204 -> 234,908
253,749 -> 308,749
704,458 -> 468,458
222,95 -> 743,616
968,840 -> 123,840
491,619 -> 491,889
979,580 -> 979,459
901,193 -> 171,923
246,155 -> 246,680
711,755 -> 247,755
671,734 -> 475,734
803,783 -> 129,109
145,890 -> 920,115
463,521 -> 463,700
782,99 -> 782,311
547,467 -> 630,467
14,88 -> 795,869
653,899 -> 653,90
488,874 -> 488,570
93,879 -> 645,327
320,658 -> 40,938
611,246 -> 611,22
258,935 -> 258,829
931,436 -> 931,263
252,460 -> 252,461
490,382 -> 965,382
242,89 -> 242,617
271,111 -> 595,435
462,706 -> 242,486
557,328 -> 747,328
486,99 -> 486,333
156,40 -> 488,372
323,482 -> 138,297
595,539 -> 812,756
923,861 -> 377,315
934,952 -> 256,274
314,777 -> 314,12
508,47 -> 508,144
888,807 -> 701,807
745,774 -> 878,907
740,716 -> 740,215
62,43 -> 62,12
571,196 -> 454,196
568,107 -> 408,107
549,676 -> 404,676
595,573 -> 595,970
148,168 -> 193,123
763,71 -> 759,71
797,64 -> 307,64
959,984 -> 32,57
457,562 -> 634,562
127,521 -> 601,47
112,296 -> 112,120
148,755 -> 451,755
636,494 -> 870,494
910,242 -> 945,277
912,911 -> 912,892
759,815 -> 759,314
391,285 -> 391,959
455,460 -> 182,460
112,78 -> 112,385
842,179 -> 842,592
236,424 -> 421,424
508,907 -> 30,907
637,219 -> 34,822
503,375 -> 503,205
570,533 -> 626,533
658,11 -> 658,94
179,286 -> 326,433
918,214 -> 200,932
339,887 -> 81,887
794,91 -> 50,835
225,356 -> 225,261
80,160 -> 80,335
148,64 -> 847,763
595,393 -> 941,393"""
