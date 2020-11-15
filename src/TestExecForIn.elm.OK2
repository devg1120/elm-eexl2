module TestExecForIn exposing (..)


import Parser exposing (..)
import Char
import Array
import Dict
import Set

--import TestExpr exposing (..)
import TestExprArrayDict exposing (..)

{--
 run : Parser a -> String -> Result (List DeadEnd) a

--}


---------------------------------------------------------------------
type alias Name =
    String

type Statement
    = Var Name
    | If Expr (List Statement) (List Statement)
    | While Expr (List Statement) 
    --| For Statement Statement (List Statement) 
    | For Expr Expr (List Statement) 
    | Assign Expr Expr
    --| Assign Statement Expr
    | Blank

{--
type Statement
    = Var Name
    | If Statement (List Statement) (List Statement)
    | While Statement (List Statement) 
    | For Statement Statement (List Statement) 
    | Assign Statement Statement
    | Blank
--}

script : Parser (List Statement)
script =
   statements
statements : Parser (List Statement)
statements =
    loop [] statementsHelp
     
spaces : Parser ()
spaces =
  chompWhile (\c -> c == ' ' || c == '\n' || c == '\r')



spsWorkAround : Parser ()
spsWorkAround =
   loop 0 <| ifProgress <|
   oneOf
   [ lineCommentWorkAround "//"
   , multiComment "/*" "*/" Nestable
   , spaces
   ]

lineCommentWorkAround : String -> Parser ()
lineCommentWorkAround start =
    succeed () |. symbol start |. chompWhile (\c -> c /= '\n')

ifProgress : Parser a -> Int -> Parser (Step Int ())
ifProgress p offset =
  succeed identity
    |. p
    |= getOffset
    |> map (\newOffset -> if offset == newOffset then Done () else Loop newOffset)



statementsHelp : List Statement -> Parser (Step (List Statement) (List Statement))
statementsHelp revStmts =
    oneOf
      [ succeed (\stmt -> Loop (stmt :: revStmts))
          |. spsWorkAround
          |= statement
          |. spsWorkAround
          |. spaces
--          |. symbol ";"
--          |. spaces
      , succeed ()
          |> map (\_ -> Done (List.reverse revStmts))
          --|> map (\_ -> Done ("--" ++ str ++ "--"))
      ]


statementsHelp2 : List Statement -> Parser (Step (List Statement) (List Statement))
statementsHelp2 revStmts =
    oneOf
      [ succeed (\stmt -> Loop (stmt :: revStmts))
          |= statement
          |. spaces
--          |. symbol ";"
--          |. spaces
      , succeed ()
          |> map (\_ -> Done (List.reverse revStmts))
          --|> map (\_ -> Done ("--" ++ str ++ "--"))
      ]


typeName : Parser Statement
typeName =
     succeed Var
        |= variable
           { start = Char.isLower
           , inner = \c -> Char.isAlphaNum c || c == '_'
           , reserved = Set.fromList [ "if", "then", "else","end", "while", "do","in", "for"  ]
           }



typeVar : Parser Statement
typeVar =
     succeed Var
        |= variable
          { start = Char.isLower
          , inner = \c -> Char.isAlphaNum c || c == '_'
          --, reserved = Set.fromList [ "let", "in", "case", "of" ]
          , reserved = Set.fromList [ "if", "then", "else","end", "while", "do","in", "for"  ]
          }


statement : Parser Statement
statement =
   oneOf
      [ assignStatement
      , ifStatement
      , whileStatement
      , forStatement
      ]


assignStatement : Parser Statement 
assignStatement =
  succeed Assign
    |. spaces
    --|= typeName
    |= expression
    |. spaces
    |. symbol "="
    |. spaces
    --|= typeVar
    |= expression
    |. spaces
    |. symbol ";"

ifStatement : Parser Statement 
ifStatement =
  succeed  If
    |. spaces
    |. keyword "if"
    |. spaces
    --|= lazy (\_ -> typeVar)
    |= lazy (\_ -> expression)
    |. spaces
    |. keyword "then"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "else"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "end"
    |. spaces

whileStatement : Parser Statement 
whileStatement =
  succeed  While
    |. spaces
    |. keyword "while"
    |. spaces
    --|= lazy (\_ -> typeVar)
    |= lazy (\_ -> expression)
    |. spaces
    |. symbol "do"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. symbol "end"
    |. spaces


forStatement : Parser Statement 
forStatement =
  succeed  For
    |. spaces
    |. keyword "for"
    |. spaces
    --|= lazy (\_ -> typeVar)
    |= lazy (\_ -> expression)
    |. spaces
    |. keyword "in"
    |. spaces
    --|= lazy (\_ -> typeVar)
    |= lazy (\_ -> expression)
    |. spaces
    |. keyword "do"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "end"
    |. spaces

input = """
   Aaa,a12;
Baa,b12;
Baa,b12;
if ccc then ddd else eee;
"""

-- run : Result (List Parser.DeadEnd) (List Statement)
-- r005 = run script input2

input5 = """
   aaa = 100;
   bbb = 200;

/*
 2224
*/




"""

input3 = """
   total = 0;

   for ok in [1,2,3,100] do

    total = total + ok;
    //total =  ok;


   
   end

   aaa = 100;
   bbb = 200;
   ccc = "XYZ";
   ddd = [1,2,3];
   eee = { "p1" : 10, "p2" : 20 , "p3" : 30};


   if aaa > bbb then
      aa = 1;
      bb = 2;
    else 
      aa = 3;
      bb=  4;
      cc = aa + bb;
   end

   f1 = 1;
   f2 = 10;
   cnt = 0;

   while f1 < f2 do
       cnt = cnt + 1;
       f1 = f1 + 1;
   end

   f1 = 1;
   ccc = [100,101,102,103,104];

   
//ok
  /*
   --
      if ccc then
         ccc = ddd
         ccc = ddd
      else 
         ddd = aaa
         ddd = bbb
      end
      if ccc then
         ccc = ddd
         ccc = ddd

      else 
         ddd = aaa
         ddd = eee
      end
*/
/*
  aaa = a11
   baa = b12
   baa = b13


   if True then
      ccc = ddd
    else 
      aaa = wsws
      ddd=  b12
      ddd = c23
      aaa = b22 + a22
   end
  

   while aaa > bbb do
   
      ddd = a
      ddd = a
   end

   for test in range do
   
      str =  zzzz
      ddd = a
      ddd = a
   end
*/
/*
   for test in range do
      ddd = a
      ddd = a
      if ccc then
         ccc = ddd
         ccc = ddd

      else 
         ddd = eee
         ddd = eee
      end
   end
*/

"""
input4 = """
  aaa = a11;
   baa = b12;
   baa = b13;

   if True then
      ccc = dd1;
   else 
      ddd = eee;
      ddd = eee;
   end;
  
   while test1 do
   
      ddd = a;
      ddd = a;
   end;
/*
   for test in range do
   
      str =  zzzz;
      ddd = a;
      ddd = a;
   end;

   for test in range do
      ddd = a;
      ddd = a;
      if ccc then
         ccc = ddd;
         ccc = ddd;

      else 
         ddd = eee;
         ddd = eee;
      end;
   end;
*/

"""
parse =
   run script input3
   --run script input4

evalWhile : Expr -> (Array.Array Statement)  -> Context -> Context
evalWhile expr arr  context =
     let
        expr_ = evaluate context expr
     in
     case expr_ of
            OBool True ->  
                 let
                   context_3 = evalStep  arr 0 context
                 in
                 evalWhile expr arr  context_3

            OBool False -> 
                 context
            _ -> 
                 context

{--
myFoldl : (a -> b -> b) -> b -> List a -> b
myFoldl func acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            myFoldl func (func x acc) xs
--}
{--
evalForHelp : (a -> b -> b) -> b -> List a -> b
evalForHelp func acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            myFoldl func (func x acc) xs
--}

evalForHelp : String -> Array.Array OutVal ->  (Array.Array Statement)  -> Context -> Context
evalForHelp name array stmt context =
        let
{--
          context_ =  case (Array.get 0 array) of
                             Just (OFloat a) ->
                               addConstant name  (OFloat a) context
                             _ ->
                               context
--}

          context_ =  case (Array.get 0 array) of
                             Just a ->
                               addConstant name  a context
                             _ ->
                               context

          context_3 = evalStep  stmt 0 context_

          array2 = Array.slice 1 (Array.length array) array
        in
        if (Array.length array2) > 0 then
           evalForHelp name array2 stmt context_3
        else
           context_3

evalFor : Expr -> Expr -> (Array.Array Statement)  -> Context -> Context
evalFor val array stmt  context =
     let
        --val_  = evaluate context val
        val2 = case val of
                  --OString a ->
                  Variable a ->
                          a
                  _ ->
                          "not"

        array_ = evaluate context array
     in
     case array_ of
            OArray a_ ->  
                 evalForHelp val2 a_ stmt  context

            _ -> 
                 context


evalStep : (Array.Array Statement) -> Int -> Context -> Context
evalStep arr pos context =
    let
      _ = Debug.log (Debug.toString pos)
      context_ = case (Array.get pos arr) of
        Just (Assign a b) ->
            let
               b_ = evaluate context b
               name = case a of
                     Variable n ->
                               n
                     _ ->
                               "undef"
            in         
            addConstant name b_ context  
        Just (If a b c) ->
            let
               a_ = evaluate context a
               name = case a_ of
                     OBool True ->  -- then
                               "true"
                     OBool False -> -- else
                               "false"
                     OString s ->
                               "false"
                     OFloat f ->
                               "false"
                     OArray _ ->
                               "false"
                     ODict _ ->
                               "false"

               context_2 = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList b) 0 context
                     OBool False -> -- else
                              evalStep (Array.fromList c) 0 context
                     OString s ->
                              context
                     OFloat f ->
                              context
                     OArray _ ->
                              context
                     ODict _ ->
                              context
            in         
            --addConstant name a_ context_2 
            context_2 

        Just (While a b ) ->
            evalWhile a (Array.fromList b) context


        Just (For a b c) ->
            evalFor a b (Array.fromList c) context

        _ ->
            context
    in
    if Array.length arr <= pos then
       context_
    else
       evalStep arr (pos + 1) context_


eval : Context -> List Statement -> OutVal
eval context stmts =
      --OString "EVAL"
    let
      _ = Debug.log (Debug.toString stmts)
      arr = Array.fromList stmts
      {--
      result = case (Array.get 0 arr) of
                   Just (Assign a b) ->
                      Debug.toString a
                   _ ->
                      ":"
      --}
      context_ = evalStep arr 0 context
    in
      --OString result
      OString (Debug.toString context_)

exec =
  let
     ast = run script input3
     result = case ast of
          Err err ->
              Debug.toString err
          Ok  stmts ->
              let
                 context = empty
                 ans = eval context stmts
              in
              Debug.toString ans
  in
  result
   
--------------------------------------------------
--eval2 : Context -> List Statement -> OutVal
eval2 : Context -> List Statement -> Context
eval2 context stmts =
      --OString "EVAL"
    let
      _ = Debug.log (Debug.toString stmts)
      arr = Array.fromList stmts
      {--
      result = case (Array.get 0 arr) of
                   Just (Assign a b) ->
                      Debug.toString a
                   _ ->
                      ":"
      --}
      context_ = evalStep arr 0 context
    in
      --OString result
      --OString (Debug.toString context_)
      context_

exec2 =
  let
     ast = run script input3
     result = case ast of
          Err err ->
              Debug.toString err
          Ok  stmts ->
              let
                 context = empty
                 ans = eval2 context stmts

                 ans2 = case ans of
                            Context a -> 
                                     a
                         --_ ->
                         --    --{ constants = {} , functions = {}}
                         --    let
                         --     (Context dmy) = empty
                         --    in
                         --   dmy

                 ans3 =  ans2.constants 
                 ans4 = Debug.toString  (Dict.toList ans3)
                            |> String.replace "\"" ""
                            |> String.replace "OFloat" ""
                            |> String.replace "[" ""
                            |> String.replace "]" ""
                            |> String.replace "),(" ") ("
                 
              in
              --Debug.toString (Dict.get "constants" ans2)
              --(Debug.toString   (Dict.toList ans3))
              ans4
  in
  result
-- r006 = run script input3
