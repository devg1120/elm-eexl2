module TestExecScorped exposing (..)


import Parser exposing (..)
import Char
import Array
import Dict
import Set

--import TestExpr exposing (..)
--import TestExprArrayDict exposing (..)
import TestExprScorped exposing (..)



---------------------------------------------------------------------
type alias Name =
    String

type Statement
    = Var Name
    | IfThen Expr (List Statement) 
    | IfThenElse Expr (List Statement) (List Statement)
    | IfThenElsIfThen Expr (List Statement) (List (Expr,(List Statement)))
    | IfThenElsIfThenElse Expr (List Statement) (List (Expr,(List Statement))) (List Statement)
    | Case        Expr  (List (Expr,(List Statement)))
    | While Expr (List Statement) 
    | For Expr Expr (List Statement) 
    | Assign Expr Expr
    | Blank


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
           , reserved = Set.fromList [ "if", "then", "else","elsif", "end", "while", "do","in", "for", "case", "default"  ]
           }



typeVar : Parser Statement
typeVar =
     succeed Var
        |= variable
          { start = Char.isLower
          , inner = \c -> Char.isAlphaNum c || c == '_'
          --, reserved = Set.fromList [ "let", "in", "case", "default", "of" ]
          , reserved = Set.fromList [ "if", "then", "else","elsif","end", "while", "do","in", "for" , "case", "default" ]
          }


statement : Parser Statement
statement =
   oneOf
      [ assignStatement
      , backtrackable ifStatement2  -- if then
      , backtrackable ifStatement   -- if then else
      , backtrackable ifStatement3  -- if then elsif else
      , backtrackable ifStatement4  -- if then elsif
      , backtrackable caseStatement -- case
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
  succeed  IfThenElse
    |. spaces
    |. keyword "if"
    |. spaces
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

ifStatement2 : Parser Statement 
ifStatement2 =
  succeed  IfThen
    |. spaces
    |. keyword "if"
    |. spaces
    |= lazy (\_ -> expression)
    |. spaces
    |. keyword "then"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "end"
    |. spaces


elsIfBlock :  Parser (Expr,(List Statement))
elsIfBlock  =
  succeed Tuple.pair
    |. spaces
    |. keyword "elsif"
    |. spaces
    |= lazy (\_ -> expression)
    |. spaces
    |. keyword "then"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
   -- |> andThen
   --      (\( expr, stmts) ->
   --           (expr, stmts)
   --      )

elsIfBlocks :  Parser (List (Expr,(List Statement)))
elsIfBlocks  =
  succeed (::)
    |. spaces
    |= lazy (\_ -> elsIfBlock)
    |. spaces
    |= elsIfBlocksTail 
    |> andThen
            (\( b ) ->
                 succeed (b)
            )

elsIfBlocksTail :  Parser (List (Expr,(List Statement)))
elsIfBlocksTail  =
  oneOf
    [ succeed (::)
        |. spaces
        |= lazy (\_ -> elsIfBlock)
        |. spaces
        |= lazy (\_ ->  elsIfBlocksTail )
    , succeed []
    ]

ifStatement3 : Parser Statement 
ifStatement3 =
  succeed  IfThenElsIfThenElse
    |. spaces
    |. keyword "if"
    |. spaces
    |= lazy (\_ -> expression)
    |. spaces
    |. keyword "then"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |= lazy (\_ -> elsIfBlocks)
    |. spaces
    |. keyword "else"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "end"
    |. spaces

ifStatement4 : Parser Statement 
ifStatement4 =
  succeed  IfThenElsIfThen
    |. spaces
    |. keyword "if"
    |. spaces
    |= lazy (\_ -> expression)
    |. spaces
    |. keyword "then"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |= lazy (\_ -> elsIfBlocks)
    |. spaces
    --|. keyword "else"
    --|. spaces
    --|= lazy (\_ -> statements)
    --|. spaces
    |. keyword "end"
    |. spaces

caseStatement : Parser Statement 
caseStatement =
  succeed  Case
    |. spaces
    |. keyword "case"
    |. spaces
    |= lazy (\_ -> expression)
    |. spaces
    |. keyword "do"
    |. spaces
    |= lazy (\_ -> caseBlocks)
    |. spaces
    |. keyword "end"
    |. spaces

caseBlock :  Parser (Expr,(List Statement))
caseBlock  =
  succeed Tuple.pair
    |. spaces
    |= lazy (\_ -> expression)
    |. spaces
    |. symbol ":"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces


caseBlocks :  Parser (List (Expr,(List Statement)))
caseBlocks  =
  succeed (::)
    |. spaces
    |= lazy (\_ -> caseBlock)
    |. spaces
    |= caseBlocksTail 
    |> andThen
            (\( b ) ->
                 succeed (b)
            )

caseBlocksTail :  Parser (List (Expr,(List Statement)))
caseBlocksTail  =
  oneOf
    [ succeed (::)
        |. spaces
        |= lazy (\_ -> caseBlock)
        |. spaces
        |= lazy (\_ ->  caseBlocksTail )
    , succeed []
    ]

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

------------------------------------------------------------------

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


evalForHelp : String -> Array.Array OutVal ->  (Array.Array Statement)  -> Context -> Context
evalForHelp name array stmt context =
        let
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

evalElsIfs :  (List (Expr,(List Statement))) -> (List Statement) -> Context -> Context
evalElsIfs   exprStmts  elseStmts  context =
         let
             (expr, stmts ) = case (List.head exprStmts) of
                                     Just a -> 
                                              a
                                     _ ->
                                              (Bool False, [])
             a_ = evaluate context expr
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
                              evalStep (Array.fromList stmts) 0 context
                     OBool False -> -- else
                              let
                                new_exprStmts = List.drop 1 exprStmts
                              in
                              if (List.length new_exprStmts) > 0 then
                                 evalElsIfs   new_exprStmts  elseStmts context

                              else
                                 evalStep (Array.fromList elseStmts) 0 context

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


evalElsIfs2 :  (List (Expr,(List Statement))) ->  Context -> Context
evalElsIfs2   exprStmts   context =
         let
             (expr, stmts ) = case (List.head exprStmts) of
                                     Just a -> 
                                              a
                                     _ ->
                                              (Bool False, [])
             a_ = evaluate context expr
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
                              evalStep (Array.fromList stmts) 0 context
                     OBool False -> -- else
                              let
                                new_exprStmts = List.drop 1 exprStmts
                              in
                              if (List.length new_exprStmts) > 0 then
                                 evalElsIfs2   new_exprStmts  context

                              else
                                 context

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

evalCaseSwitch :  OutVal -> (List (Expr,(List Statement))) ->  Context -> Context
evalCaseSwitch   target exprStmts   context =
         let
             (expr, stmts ) = case (List.head exprStmts) of
                                     Just a -> 
                                              a
                                     _ ->
                                              (Bool False, [])
             a_ = evaluate context expr
             --cond = target == a_

             cond = case expr of 
                         Default a ->
                              True
                         _ ->
                              target == a_

             context_2 = case cond of
                      True ->  
                              evalStep (Array.fromList stmts) 0 context
                      False -> 
                              let
                                new_exprStmts = List.drop 1 exprStmts
                              in
                              if (List.length new_exprStmts) > 0 then
                                 evalCaseSwitch target  new_exprStmts  context

                              else
                                 context
            in         
            context_2 

scopePush : Context -> Context
scopePush context =
           let
            context_record = case context of
                        Context con -> 
                                 con
           in
           if context_record.scope then
                   let
                     ct = dicPush context_record.constants
                   in
                   Context
                     {
                      context_record
                       | constants  = ct
                       }
           else
                   context

scopePop : Context -> Context
scopePop context =
           let
            context_record = case context of
                        Context con -> 
                                 con
           in
           if context_record.scope then
                   let
                     ct = dicPop context_record.constants
                   in
                   Context
                     {
                      context_record
                       | constants  = ct
                       }
           else
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
        Just (IfThenElse a b c) ->
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
{--
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
            context_2 
--}
               context_1 = scopePush context
               context_2 = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList b) 0 context_1
                     OBool False -> -- else
                              evalStep (Array.fromList c) 0 context_1
                     OString s ->
                              context_1
                     OFloat f ->
                              context_1
                     OArray _ ->
                              context_1
                     ODict _ ->
                              context_1


               context_3 = scopePop context_2
            in         
            context_3 


        Just (IfThen a b ) ->
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

               context_1 = scopePush context
               context_2 = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList b) 0 context_1
                     OBool False -> -- else
                              context_1
                     OString s ->
                              context_1
                     OFloat f ->
                              context_1
                     OArray _ ->
                              context_1
                     ODict _ ->
                              context_1
               context_3 = scopePop context_2
            in         
            context_3 

        Just (IfThenElsIfThenElse a b c d) ->
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

               context_1 = scopePush context
               context_2 = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList b) 0 context_1
                     OBool False -> -- else
                              evalElsIfs c d context_1
                     OString s ->
                              context_1
                     OFloat f ->
                              context_1
                     OArray _ ->
                              context_1
                     ODict _ ->
                              context_1
               context_3 = scopePop context_2
            in         
            context_3 

        Just (IfThenElsIfThen a b c ) ->
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

               context_1 = scopePush context
               context_2 = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList b) 0 context_1
                     OBool False -> -- else
                              evalElsIfs2 c  context_1
                     OString s ->
                              context_1
                     OFloat f ->
                              context_1
                     OArray _ ->
                              context_1
                     ODict _ ->
                              context_1
               context_3 = scopePop context_2
            in         
            context_3 

        Just (Case a b  ) ->
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

               context_1 = scopePush context
               context_2 = evalCaseSwitch a_ b  context_1
               context_3 = scopePop context_2

            in         
            context_3 

        Just (While a b ) ->
            let
               context_1 = scopePush context
               context_2 = evalWhile a (Array.fromList b) context_1
               context_3 = scopePop context_2
            in         
            context_3 


        Just (For a b c) ->
            let
               context_1 = scopePush context
               context_2 = evalFor a b (Array.fromList c) context_1
               context_3 = scopePop context_2
            in         
            context_3 

        _ ->
            context
    in
    if Array.length arr <= pos then
       context_
    else
       evalStep arr (pos + 1) context_

----------------------------------------------------------
parse script_name =
   run script script_name

eval1 : Context -> List Statement -> OutVal
eval1 context stmts =
    let
      _ = Debug.log (Debug.toString stmts)
      arr = Array.fromList stmts
      context_ = evalStep arr 0 context
    in
      OString (Debug.toString context_)

exec1 script_name =
  let
     ast = run script script_name
     result = case ast of
          Err err ->
              Debug.toString err
          Ok  stmts ->
              let
                 context = empty
                 ans = eval1 context stmts
              in
              Debug.toString ans
  in
  result
   
--------------------------------------------------
eval2 : Context -> List Statement -> Context
eval2 context stmts =
      --OString "EVAL"
    let
      _ = Debug.log (Debug.toString stmts)
      arr = Array.fromList stmts
      context_ = evalStep arr 0 context
    in
      context_

---------------------------------------------------
exec2 script_name =
  let
     ast = run script script_name
     result = case ast of
          Err err ->
              Debug.toString err
          Ok  stmts ->
              let
                 context = empty
                             |> addFunction "strjoin" strjoin 

                 ans = eval2 context stmts

                 ans2 = case ans of
                            Context a -> 
                                     a

                 ans3 =  ans2.constants 
                 log =  ans2.log
                 --ans4 = Debug.toString  (Dict.toList ans3)
                 --           |> String.replace "\"" ""
                 --           |> String.replace "OFloat" ""
                 --           |> String.replace "[" ""
                 --           |> String.replace "]" ""
                 --           |> String.replace "),(" ") ("
                 
                 ans4 = Debug.toString  (ans3)
                            |> String.replace "\"" ""
                            |> String.replace "OFloat" ""
                            |> String.replace "[" ""
                            |> String.replace "]" ""
                            |> String.replace "," " ="
                            |> String.replace ") =(" " , "
                            --|> String.replace "(" ""
                            --|> String.replace ")" ""
                            --|> String.replace "Stack Dict.fromList " ""
              in
              ans4 ++ " -> " ++ log
  in
  result


------------------------------------------------------------------
input3 = """
   total = 0;


   aaa = 100;
   bbb = 200;
   ccc = 300;
   aa = 0;
   bb = 0;
   cc = 0;


   if aaa < bbb then
      aa = 1;
    else 
      bb=  1;
   end

   if aaa < bbb then
      aa = aa + 2;
   end


   za = 100;
   zb = 200;
   zc = 300;
   zd = 400;

   z = 0;

   if za > zb then
      z = 1;
    elsif  za > zc then
      z=  2;
    elsif  za > zd then
      z=  3;
    else 
      z=  4;
   end


   xa = 350;
   xb = 200;
   xc = 300;
   xd = 400;

   x = 0;

   if xa < xb then
      x = 1;
    elsif  xa < xc then
      x=  2;
    elsif  xa < xd then
      x=  3;
   end

   ca = 6;
   re = 0;
   case ca do
       0:
          re = 1;
       1:
          re = 2;
       2:
          re = 3;
   
       default:
        re = 5;
   end


"""

input33 = """
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
script1 = """
  a = 1;
  b = 0;

  if a > 1 then
     b = 1;
  else
     b = 2;
     c = 1;
  end
   a = 100;

  c = strjoin("ABC", "abc");

"""
help = """
parse input3
exec1 input3
exed2 input3
"""
