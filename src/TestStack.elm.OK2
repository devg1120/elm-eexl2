module TestStack exposing (..)

import Array
import Dict
import Stack

{--
s = Stack.initialise 

s2 = Stack.push 1 s
          |> Stack.push 2 
          |> Stack.push 3 
          |> Stack.push 4 
          |> Stack.push 5 
          |> Stack.push 6 
          |> Stack.push 7 
          |> Stack.push 8 
          |> Stack.push 9 

res = Stack.pop  s2
r = Tuple.first res
s3 = Tuple.second res

------------------------------
d = Stack.initialise 

dx = Stack.push Dict.empty d
          |> Stack.push  Dict.empty
          |> Stack.push  Dict.empty
          |> Stack.push  Dict.empty


dres = Stack.pop  dx
dxr = Tuple.first dres
dx3 = Tuple.second dres
--}
------------------------------------------- Dict Stack  add set
{--
dicA = Dict.empty
        |> Dict.insert "A" 1
dicB = Dict.empty
        |> Dict.insert "B" 2
dicC = Dict.empty
        |> Dict.insert "C" 3
dicD = Dict.empty
        |> Dict.insert "D" 4

dic = Stack.initialise 
        |> Stack.push dicA
        |> Stack.push dicB
        |> Stack.push dicC
        |> Stack.push dicD
--}
----------------------------------
dicGetSerch : List (Dict.Dict String Int) -> String -> Result String Int
dicGetSerch list name =
     let
        dict =  List.head list  |> Maybe.withDefault Dict.empty
        value = Dict.get name dict
     in
       case value of
          Just a ->
                Ok  a
          _ ->
             let
               new_list = List.drop 1 list
             in
             if List.isEmpty new_list then
                Err ("dicGetSerch...not found:" ++ name)
             else
                dicGetSerch new_list name

                 


dicGet : String -> Stack.Stack (Dict.Dict String Int) -> Result String Int
dicGet name  stackdic =
      let
       list = Stack.toList stackdic
      in
      dicGetSerch list name




dicSetUpdate : String -> Int ->  Stack.Stack (Dict.Dict String Int) ->  Result String (Stack.Stack (Dict.Dict String Int))
dicSetUpdate name value stackdic =
     let
        (dict, stack_) =  Stack.pop stackdic  
        dict2 = case dict of
                   Just dict_ ->
                            dict_
                   _ ->
                            Dict.empty
        value2 = Dict.get name dict2
     in
       case value2 of
          Just a ->
                let 
                 tmp_dict = Dict.insert name value dict2
                in
                Ok (Stack.push tmp_dict stack_)
          _ ->
             let
               tmp_list = Stack.toList stack_

             in
             if List.isEmpty tmp_list then
                Err ("dicSetUpdate...not found:" ++ name)
             else
              let
                new_stack_pair = dicSetUpdate  name value stack_ 
              in
                case new_stack_pair of
                    Ok new_stack_ ->
                          Ok (Stack.push dict2 new_stack_)
                    Err str ->
                          Err str

dicSet : String -> Int -> Stack.Stack (Dict.Dict String Int) -> Result String (Stack.Stack (Dict.Dict String Int))
dicSet name value stackdic =
      let
       result = dicSetUpdate name value stackdic 
      in
      result


dicSetNewLocal : String -> Int -> Stack.Stack (Dict.Dict String Int) -> Result String (Stack.Stack (Dict.Dict String Int))
dicSetNewLocal name value stackdic =
     let
        (dict, stack_) =  Stack.pop stackdic  
        dict2 = case dict of
                   Just dict_ ->
                            dict_
                   _ ->
                            Dict.empty
        value2 = Dict.insert name value dict2
     in
       Ok (Stack.push value2 stack_)

dicPop : Stack.Stack (Dict.Dict String Int) ->Stack.Stack (Dict.Dict String Int)
dicPop stackdic =
      let
       (a, newdic) = Stack.pop stackdic
      in
       newdic

dicPush : Stack.Stack (Dict.Dict String Int) ->Stack.Stack (Dict.Dict String Int)
dicPush stackdic =
     let
        dict = Dict.empty
     in
       Stack.push dict stackdic


----------------------------------------------------
type OutVal
  = OFloat  Float
  | OInt Int
  | OString String
  | OBool Bool
  | OArray (Array.Array OutVal)
  | ODict  (Dict.Dict String OutVal)  

dicInit :  Stack.Stack (Dict.Dict String OutVal)
dicInit  =
     Stack.initialise

----------------------------------------------------

dicA = Dict.empty
        |> Dict.insert "a" 1
dicB = Dict.empty
        |> Dict.insert "B" 2
dicC = Dict.empty
        |> Dict.insert "C" 3
dicD = Dict.empty
        |> Dict.insert "D" 4

dic = Stack.initialise 
        |> Stack.push dicA
        |> Stack.push dicB
        |> Stack.push dicC
        |> Stack.push dicD

rr1 = dicGet "A"  dic
rr2 = dicGet "B"  dic
rr3 = dicGet "C"  dic
rr4 = dicGet "D"  dic
rr5 = dicGet "a"  dic

rr6 = dicSet "D" 9  dic
rr7 = dicSet "a" 8  dic
rr8 = dicSetNewLocal "a" 8  dic
t1 = dicPop dic
t2 = dicPush t1

------------------------------------
{--
d = dicInit

d1 = dicSet "aaa" (OInt  9) d
--}
