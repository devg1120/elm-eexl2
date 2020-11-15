module Test exposing (..)

--import Eexl.Context as Context exposing (Context)
import Eexl.Context as Context exposing (Context,Input (..))
import Eexl.Eexl exposing (evaluateBool, evaluateInt)
import Eexl.Parse exposing (parse)
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

r31 = parse (Context.empty |> Context.addConstant "x" 9) "1 + x"      --10

{--
add : String -> Int
add a  =
   let
     a_ = String.toInt a  |> Maybe.withDefault 0 
   in
   a_ 


r41 = parse (Context.empty |> Context.addFunction "add" add ) "add(\"9\")"      --9
--}


{--
add :(Array.Array String) -> Int
add ar  =
   let
     a_ = String.toInt (Array.get 0 ar  |> Maybe.withDefault "0") |> Maybe.withDefault 0 
     b_ = String.toInt (Array.get 1 ar  |> Maybe.withDefault "0") |> Maybe.withDefault 0
   in
    a_ + b_


r41 = parse (Context.empty |> Context.addFunction "add" add ) "add(\"9\",\"2\")"      --11


formula2 = """ 
    add("222","111")

"""
r42 = parse (Context.empty |> Context.addFunction "add" add ) formula2     --9

--}

str2intadd :Input -> T
str2intadd ar  =
   let
     ans = case ar of
              ArrayString ar_ ->
                 let
                   a_ = String.toInt (Array.get 0 ar_  |> Maybe.withDefault "0") |> Maybe.withDefault 0 
                   b_ = String.toInt (Array.get 1 ar_  |> Maybe.withDefault "0") |> Maybe.withDefault 0
                 in
                 a_ + b_

              _ ->
                 -1
   in
   IntT ans

intadd :Input -> T
intadd ar  =
   let _ = Debug.log "call initadd" 0 in
   let
     ans = case ar of
              ArrayInt ar_ ->
                 let
                   a_ = Array.get 0 ar_  |> Maybe.withDefault 0 
                   b_ = Array.get 1 ar_  |> Maybe.withDefault 0
                 in
                 a_ + b_

              _ ->
                 -1
   in
   IntT ans

fladd :Input -> T
fladd ar  =
   let _ = Debug.log "call fladd" 0 in
   let
     ans = case ar of
              ArrayFloat ar_ ->
                 let
                   a_ = Array.get 0 ar_  |> Maybe.withDefault 0 
                   b_ = Array.get 1 ar_  |> Maybe.withDefault 0
                 in
                 a_ + b_

              _ ->
                 -1.0
   in
   FloatT ans

strjoin :Input -> T
strjoin ar  =
   let
     ans = case ar of
              ArrayString ar_ ->
                 let
                   a_ = Array.get 0 ar_  |> Maybe.withDefault "*"  
                   b_ = Array.get 1 ar_  |> Maybe.withDefault "*"  
                 in
                 a_ ++ b_

              _ ->
                 "not join"
   in
   StringT ans


---------------------------------------

r41 = parse (Context.empty |> Context.addFunction "str2intadd" str2intadd ) "str2intadd(\"9\", \"2\")"      --11
r42 = parse (Context.empty |> Context.addFunction "intadd" intadd ) "intadd( 7,2)"      --11

--r43 = parse (Context.empty |> Context.addFunction "strjoin" strjoin ) "strjoin(\"3.1\",  \"10.55\")"      --11
r43 = parse (Context.empty |> Context.addFunction "fladd" fladd ) "fladd( 3.1,10.55 )"      --11


formula2 = """ 
    str2intadd("222","111")

"""
r44 = parse (Context.empty |> Context.addFunction "str2intadd" str2intadd ) formula2     --333

r45 = parse (Context.empty |> Context.addFunction "strjoin" strjoin ) "strjoin(  \"AAA\" , \"BBB\")"      --AAABBB
r46 = parse (Context.empty |> Context.addFunction "strjoin" strjoin ) "strjoin(\"1.1\",\" 1.2\")"      --AAABBB


