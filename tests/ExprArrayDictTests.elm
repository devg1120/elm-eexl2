module ExprArrayDictTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (Problem(..))
import TestExprArrayDict exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "TestExprArrayDict"
        [ literalTests  --14
        , funcTests     -- 4
        , compTests     -- 7    
        , arrayTests    -- 5
        , dictTests     -- 5
        ]



eqt ans test =
         Expect.equal ans  test

eqt2 test ans =
         Expect.equal ans  test

literalTests : Test
literalTests =
    describe "literals"
       [ test "01" <| \_ ->  eqt ( "OFloat 7"           )  ( exec " 1 + 3 + (7 // 2)"  ) 
       , test "02" <| \_ ->  eqt ( "OFloat 7.5"         )  ( exec " 1 + 3 + (7 /  2)"  ) 
       , test "03" <| \_ ->  eqt ( "OFloat 5"           )  ( exec " 1 + 3 + (7 %  2)"  ) 
       , test "04" <| \_ ->  eqt ( "OString \"abcABC\"" )  ( exec " \"abc\" + \"ABC\"" ) 
       , test "05" <| \_ ->  eqt ( "OBool False"        )  ( exec "False" ) 
       , test "06" <| \_ ->  eqt ( "OBool True"         )  ( exec "True"  ) 
       , test "07" <| \_ ->  eqt ( "OBool True"         )  ( exec "True  && True"  ) 
       , test "08" <| \_ ->  eqt ( "OBool False"        )  ( exec "True  && False" ) 
       , test "09" <| \_ ->  eqt ( "OBool False"        )  ( exec "False && True"  ) 
       , test "10" <| \_ ->  eqt ( "OBool False"        )  ( exec "False && False" ) 
       , test "11" <| \_ ->  eqt ( "OBool True"         )  ( exec "True  || True"  ) 
       , test "12" <| \_ ->  eqt ( "OBool True"         )  ( exec "True  || False" ) 
       , test "13" <| \_ ->  eqt ( "OBool True"         )  ( exec "False || True"  ) 
       , test "14" <| \_ ->  eqt ( "OBool False"        )  ( exec "False || False" ) 
       ]

funcTests : Test
funcTests =
    describe "func"
       [ test "01" <| \_ ->  eqt ( "OString \"abcOKOK\""    )  ( exec " \"abc\" + test1 "  ) 
       , test "02" <| \_ ->  eqt ( "OFloat 11.2"            )  ( exec " 1.1  + test_flort "  ) 
       , test "03" <| \_ ->  eqt ( "OString \"abcABCXYZ\""  )  ( exec " \"abc\" + strjoin( \"ABC\", \"XYZ\") " ) 
       , test "04" <| \_ ->  eqt ( "OString \"abcABCOKOK\"" )  ( exec " \"abc\" + strjoin( \"ABC\", test1)" ) 
       ]

compTests : Test
compTests =
    describe "comp"
       [ test "01" <| \_ ->  eqt ( "OBool True"         )  ( exec "1.0 <= 100.1 "  ) 
       , test "02" <| \_ ->  eqt ( "OBool True"         )  ( exec "1.0 <  100.1 "  ) 
       , test "03" <| \_ ->  eqt ( "OBool False"        )  ( exec "1.0 >  100.1 "  ) 
       , test "04" <| \_ ->  eqt ( "OBool False"        )  ( exec "1.0 >= 100.1 "  ) 
       , test "05" <| \_ ->  eqt ( "OBool False"        )  ( exec "1.0 == 100.1 "  ) 
       , test "06" <| \_ ->  eqt ( "OBool True"         )  ( exec "1.0 != 100.1 "  ) 
       , test "07" <| \_ ->  eqt ( "OBool True"         )  ( exec "1.1 == 1.1   "  ) 
       ]

arrayTests : Test
arrayTests =
    describe "array"
       [ test "01" <| \_ ->  eqt
         ( "OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])" ) 
         ( exec "[1,2,3,4,5]"  ) 

       , test "02" <| \_ ->  eqt2
         ( exec "[1,2,3,4,5]"  ) 
         ( "OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])" ) 

       , test "03" <| \_ ->  eqt2
         ( exec "[\"1\",\"2\",\"3\",\"4\",\"5\"]"  ) 
         ( "OArray (Array.fromList [OString \"1\",OString \"2\",OString \"3\",OString \"4\",OString \"5\"])" ) 

       , test "04" <| \_ ->  eqt2
         ( exec "array_test"  ) 
         ( "OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])" ) 

       , test "05" <| \_ ->  eqt2
         ( exec "array_test[1]"  ) 
         ( "OFloat 2" ) 

       ]

dictTests : Test
dictTests =
    describe "dict"
     [ test "01" <| \_ ->  eqt2
       ( exec "{\"ab\" : 1, \"xy\" : 2, \"zz\" : 9 }"  ) 
       ( "ODict (Dict.fromList [(\"ab\",OFloat 1),(\"xy\",OFloat 2),(\"zz\",OFloat 9)])" )

     , test "02" <| \_ ->  eqt2
       ( exec "{\"ab\" : \"1\", \"xy\" : \"2\", \"zz\" : \"9\" }"  ) 
       ( "ODict (Dict.fromList [(\"ab\",OString \"1\"),(\"xy\",OString \"2\"),(\"zz\",OString \"9\")])" ) 

     , test "03" <| \_ ->  eqt2
       ( exec "dict_test"  ) 
       ( "ODict (Dict.fromList [(\"a\",OFloat 1),(\"b\",OFloat 2),(\"c\",OFloat 3),(\"d\",OFloat 4),(\"e\",OFloat 5)])" ) 

     , test "04" <| \_ ->  eqt2
       ( exec "dict_test{\"c\"}"  ) 
       ( "OFloat 3" ) 

     , test "05" <| \_ ->  eqt2
       ( exec "dict_test.c"  ) 
       ( "OFloat 3" ) 
     ]
{--


arrayTests : Test
arrayTests =
    describe "array"
       [ test "01" <| \_ ->  eqt ( "OArray (Array.fromList [OFloat 1,OFloat 2,OFloat 3,OFloat 4,OFloat 5])"         )  ( exec " [ 1,2,3,4,5]"  ) 
       --, test "02" <| \_ ->  eqt ( "OBool True"         )  ( exec "1.0 <  100.1 "  ) 
       --, test "03" <| \_ ->  eqt ( "OBool False"        )  ( exec "1.0 >  100.1 "  ) 
       --, test "04" <| \_ ->  eqt ( "OBool False"        )  ( exec "1.0 >= 100.1 "  ) 
       --, test "05" <| \_ ->  eqt ( "OBool False"        )  ( exec "1.0 == 100.1 "  ) 
       --, test "06" <| \_ ->  eqt ( "OBool True"         )  ( exec "1.0 != 100.1 "  ) 
       --, test "07" <| \_ ->  eqt ( "OBool True"         )  ( exec "1.1 == 1.1   "  ) 
       ]


literalTests : Test
literalTests =
    describe "literals"
       [ test "01" <|  \_ ->  Expect.equal ( "OFloat 7" )  (exec " 1 + 3 + (7 //2)" ) 
        ]
--}
{--
        [ test "true" <|
            \_ ->
                Expect.equal
                    ( "OFloat 7" )
                    (exec " 1 + 3 + (7 //2)" )
--}
{--
        , test "false" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool Context.empty "false")
        , fuzz int "int" <|
            \n ->
                Expect.equal
                    (Ok n)
                    (evaluateInt Context.empty <| String.fromInt n)
        ]
--}

{--
mathsTests : Test
mathsTests =
    describe "Maths"
        [ test "1 + 1" <|
            \_ ->
                Expect.equal
                    (Ok 2)
                    (evaluateInt Context.empty "1 + 1")
        , test "1 + 2 + 3" <|
            \_ ->
                Expect.equal
                    (Ok 6)
                    (evaluateInt Context.empty "1 + 2 + 3")
        , test "100 * 3 + 2" <|
            \_ ->
                Expect.equal
                    (Ok 302)
                    (evaluateInt Context.empty "100 * 3 + 2")
        , test "100 * (3 + 2)" <|
            \_ ->
                Expect.equal
                    (Ok 500)
                    (evaluateInt Context.empty "100 * (3 + 2)")
        ]


boolTests : Test
boolTests =
    describe "Bool"
        [ test "true" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool Context.empty "true")
        , test "1 + 2 < 4" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool Context.empty "1 + 2 < 4")
        , test "1 + 2 * 3 < 4 + 3" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool Context.empty "1 + 2 * 3 < 4 + 3")
        ]


varTests : Test
varTests =
    describe "varTests"
        [ test "non-existant var" <|
            \_ ->
                Expect.err
                    (evaluateInt Context.empty "1 + x")
        , test "existing var" <|
            \_ ->
                Expect.equal
                    (Ok 2)
                    (evaluateInt (Context.empty |> Context.addConstant "x" 1) "1 + x")
        ]


funcTests : Test
funcTests =
    let
        stringToInt : String -> Int
        stringToInt =
            String.toInt >> Maybe.withDefault -1
    in
    describe "funcTests"
        [ test "non-existant function" <|
            \_ ->
                Expect.err
                    (evaluateInt Context.empty "stringToInt(\"something\")")
        , test "function 1" <|
            \_ ->
                Expect.equal
                    (Ok 1)
                    (evaluateInt (Context.empty |> Context.addFunction "stringToInt" stringToInt) "stringToInt(\"1\")")
        , test "function 2" <|
            \_ ->
                Expect.equal
                    (Ok -1)
                    (evaluateInt (Context.empty |> Context.addFunction "stringToInt" stringToInt) "stringToInt(\"something else\")")
        ]


realWorldTests : Test
realWorldTests =
    let
        makeContext : Int -> Int -> Int -> Context
        makeContext unitTotal correct funcReturns =
            Context.empty
                |> Context.addConstant "unitTotal" unitTotal
                |> Context.addConstant "correct" correct
                |> Context.addFunction "unitScoreWithTags" (\_ -> funcReturns)
                |> Context.addFunction "unitScoreWithoutTags" (\_ -> funcReturns)
                |> Context.addFunction "exerciseScoreWithTags" (\_ -> funcReturns)
                |> Context.addFunction "exerciseScoreWithoutTags" (\_ -> funcReturns)
    in
    describe "realWorldTests"
        [ test "2 + 3 + 4" <|
            \_ ->
                Expect.equal
                    (Ok 9)
                    (evaluateInt Context.empty """2 + 3 + 4""")
        , test "1 == 2 && 3 == 4" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool Context.empty """1 == 2 && 3 == 4""")
        , test "1 + 2 < 4" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool Context.empty """1 + 2 < 4""")
        , test "1 + 2 * 3" <|
            \_ ->
                Expect.equal
                    (Ok 9)
                    (evaluateInt Context.empty """1 + 2 * 4""")
        , test "1 + 2 * 3 < 4 + 3" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool Context.empty "1 + 2 * 3 < 4 + 3")
        , test "2 + 3 ^ 2 * 3 + 4" <|
            \_ ->
                Expect.equal
                    (Ok 33)
                    (evaluateInt Context.empty "2 + 3 ^ 2 * 3 + 4")
        , test "2 + 3 * 4 + 5 == 19" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool Context.empty "2 + 3 * 4 + 5 == 19")
        , test "1.1" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool (makeContext 3 0 0) """unitTotal == 9 && unitScoreWithTags("A2") >= 1""")
        , test "1.2" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool (makeContext 9 0 1) """unitTotal == 9 && unitScoreWithTags("A2") >= 1""")
        , test "2.1" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool (makeContext 9 0 1) """unitTotal >= 10 && unitTotal <= 11""")
        , test "2.2" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool (makeContext 9 0 0) """unitTotal >= 10 && unitTotal <=11""")
        , test "2.3" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool (makeContext 10 0 0) """unitTotal >= 10 && unitTotal <=11""")
        , test "2.4" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool (makeContext 11 0 0) """unitTotal >= 10 && unitTotal <=11""")
        , test "2.5" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool (makeContext 12 0 0) """unitTotal >= 10 && unitTotal <=11""")
        , test "3.1" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool (makeContext 0 2 0) """correct <= 4 && exerciseScoreWithoutTags("A[1-2]") <= 0""")
        , test "3.2" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool (makeContext 0 4 0) """correct <= 4 && exerciseScoreWithoutTags("A[1-2]") <= 0""")
        , test "3.3" <|
            \_ ->
                Expect.equal (Ok False)
                    (evaluateBool (makeContext 0 5 0) """correct <= 4 && exerciseScoreWithoutTags("A[1-2]") <= 0""")
        , test "3.4" <|
            \_ ->
                Expect.equal (Ok False)
                    (evaluateBool (makeContext 0 0 1) """correct <= 4 && exerciseScoreWithoutTags("A[1-2]") <= 0""")
        ]

--}

{-
   correct <= 4 && (!%'A[1-2]') <= 0
   correct >= 1 && correct <= 4 && (!%'A[1-2]') >= 1
   correct >= 0 && correct <= 4 && (!%'A[1-2]') <= 0
   correct >= 1 && correct <= 4 && (!%'A[1-2]') >= 1
   correct >= 8 && correct <= 12 && (%'C[1-2]') >= 1
   unitTotal >= 5 && unitTotal <= 7 && (%unit'C[1-2]') <= 0
   unitTotal >= 5 && unitTotal <= 7 && (%unit'C[1-2]') >= 1
-}
{-

   unitTotal == 9 && (%unit'A2') >= 1
   unitTotal >= 10 && unitTotal <=11
   unitTotal >= 19
   unitTotal >= 0 && unitTotal <= 2
   unitTotal == 9 && (%unit'B2') >= 1
   unitTotal >= 19 && unitTotal <= 22
   correct <= 4 && (!%'A[1-2]') <= 0
   correct >= 1 && correct <= 4 && (!%'A[1-2]') >= 1
   correct >= 0 && correct <= 4 && (!%'A[1-2]') <= 0
   correct >= 1 && correct <= 4 && (!%'A[1-2]') >= 1
   correct >= 8 && correct <= 12 && (%'C[1-2]') >= 1
   unitTotal >= 5 && unitTotal <= 7 && (%unit'C[1-2]') <= 0
   unitTotal >= 5 && unitTotal <= 7 && (%unit'C[1-2]') >= 1
-}
