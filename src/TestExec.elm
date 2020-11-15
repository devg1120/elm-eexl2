module TestExec exposing (..)


import Parser exposing (..)
import Char
import Array
import Set

import TestExpr exposing (..)

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
    | For Statement Statement (List Statement) 
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

{--
statementsHelp : List Statement -> Parser (Step (List Statement) (List Statement))
statementsHelp revStmts =
    oneOf
      [ succeed (\stmt -> Loop (stmt :: revStmts))
          |. spsWorkAround
          |= statement
          |. spsWorkAround
          |. spaces
          |. symbol ";"
          |. spaces
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
          |. symbol ";"
          |. spaces
      , succeed ()
          |> map (\_ -> Done (List.reverse revStmts))
          --|> map (\_ -> Done ("--" ++ str ++ "--"))
      ]

--}



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
    |= lazy (\_ -> typeVar)
    |. spaces
    |. keyword "in"
    |. spaces
    |= lazy (\_ -> typeVar)
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

input3 = """
   aaa = 100
   bbb = 200
   ccc = "XYZ"


   if aaa > bbb then
      aa = 1
      bb = 2
    else 
      aa = 3
      bb=  4
      cc = aa + bb
   end

   f1 = 1
   f2 = 10
   cnt = 0

   while f1 < f2 do
       cnt = cnt + 1
       f1 = f1 + 1
   end
/*
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
               context_2 = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList b) 0 context
                     OBool False -> -- else
                              evalStep (Array.fromList c) 0 context
                     OString s ->
                              context
                     OFloat f ->
                              context
            in         
            --addConstant name a_ context_2 
            context_2 

        Just (While a b ) ->
            evalWhile a (Array.fromList b) context

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
   
-- r006 = run script input3
