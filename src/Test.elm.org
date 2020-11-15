module Test exposing (..)

import Eexl.Context as Context exposing (Context,Input , ArgValue(..) )
import Eexl.Eexl exposing (evaluateBool, evaluateInt)
import Eexl.Parse exposing (parse, parse2)
import Eexl.Eval as Eval exposing (T(..))
import Array


r11 = parse Context.empty " 1 + 2"

r12 = parse Context.empty "true"          --True
r13 = parse Context.empty "100 * (3 + 2)" --500
r14 = parse Context.empty "100 *  3 + 2 " --302
--r15 = parse Context.empty "(1 + 2) < 4" --True   NG
r16 = parse Context.empty " 1 + 2  < 4"   --True


-- multi line

formula1 = """ 1
+ 
2
"""

r21 = parse Context.empty formula1      --3
r31 = parse (Context.empty |> Context.addConstant "x" (IntT 9)) "1 + x"      --10

---------------------------------------------------------------------------------
str2intadd :Input -> T
str2intadd ar  =
   let
    
       a_ = case (Array.get 0 ar) of
                  Just (AvString a)  ->
                          String.toInt a 
                  _ ->
                          String.toInt "0"

       b_ = case (Array.get 1 ar) of
                  Just (AvString a)  ->
                          String.toInt a 
                  _ ->
                          String.toInt "0"

       ans = (a_ |> Maybe.withDefault 0) + (b_ |> Maybe.withDefault 0)
   in
   IntT ans



intadd :Input -> T
intadd ar  =
   let
       a_ = case (Array.get 0 ar) of
                  Just (AvInt a)  ->
                           a 
                  _ ->
                           0

       b_ = case (Array.get 1 ar) of
                  Just (AvInt a)  ->
                           a 
                  _ ->
                          0

       ans = a_ + b_
   in
   IntT ans


fladd :Input -> T
fladd ar  =
   let
       a_ = case (Array.get 0 ar) of
                  Just (AvFloat a)  ->
                           a 
                  _ ->
                           0

       b_ = case (Array.get 1 ar) of
                  Just (AvFloat a)  ->
                           a 
                  _ ->
                          0

       ans = a_ + b_
   in
   FloatT ans


strjoin :Input -> T
strjoin ar  =
   let
       a_ = case (Array.get 0 ar) of
                  Just (AvString a)  ->
                           a 
                  _ ->
                           ""

       b_ = case (Array.get 1 ar) of
                  Just (AvString a)  ->
                           a 
                  _ ->
                          ""

       ans = a_ ++  b_
   in
   StringT ans


substr :Input -> T
substr ar  =
   let
       str_ = case (Array.get 0 ar) of
                  Just (AvString a)  ->
                           a 
                  _ ->
                           ""

       index_ = case (Array.get 1 ar) of
                  Just (AvInt a)  ->
                           a 
                  _ ->
                          0

       length_ = case (Array.get 2 ar) of
                  Just (AvInt a)  ->
                           a 
                  _ ->
                          0

       ans = String.slice index_ (index_ + length_) str_
   in
   StringT ans

---------------------------------------------------------------------------------

r41 = parse (Context.empty |> Context.addFunction "str2intadd" str2intadd ) "str2intadd(\"9\", \"2\")"      --11

r42 = parse (Context.empty |> Context.addFunction "intadd" intadd ) "intadd( 7,2)"      --11

r43 = parse (Context.empty |> Context.addFunction "fladd" fladd ) "fladd( 3.1,10.55 )"      --11

r44 = parse (Context.empty |> Context.addFunction "strjoin" strjoin ) "strjoin(\"3.1\",  \"10.55\")"      --11


formula2 = """ 
    str2intadd("222","111")

"""
r45 = parse (Context.empty |> Context.addFunction "str2intadd" str2intadd ) formula2     --333

r46 = parse (Context.empty |> Context.addFunction "strjoin" strjoin ) "strjoin(  \"AAA\" , \"BBB\")"      --AAABBB

r47 = parse (Context.empty |> Context.addFunction "strjoin" strjoin ) "strjoin(\"1.1\",\" 1.2\")"      --AAABBB


formula3 = """ 
    substr("0123456789ABCD" , 3, 5)

"""
r51 = parse (Context.empty |> Context.addFunction "substr" substr ) formula3     --"34567"

formula4 = """ 
    intadd(5 , 3) + val

"""
r52 = parse (Context.empty 
                |> Context.addFunction "intadd"  intadd
                |> Context.addConstant "val"  (IntT 100)

     ) formula4     

formula5 = """ 
    intadd(5 , val) 

"""
r53 = parse (Context.empty 
                |> Context.addFunction "intadd"  intadd
                |> Context.addConstant "val"  (IntT 100)

     ) formula5     

---------------------------

script1 = """
    a = sum( ); 12345;
   b98765;
   sun aa
       bbb ;
"""
r100 = parse2 Context.empty  script1

script2 = """
a #     a = sum( ) 
#     b = sum( ) ;
12345;
   str =  関西;  # コメント
   "9#87#65"  xyz "__#__" "abc # d" # www;
   sun aa ;
   #   bbb  
"""
script3 = """
    a = sum( ) 
12345 ]
   b98765 ]
   sun aa ]
       bbb ] 
"""
r101 = parse2 Context.empty  script2
r102 = Debug.toString (parse2 Context.empty  script2)

