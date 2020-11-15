module TestExecScorpedDefVarFunc exposing (..)


import Parser exposing (..)
import Char
import Array
import Dict
import Set

--import TestExpr exposing (..)
--import TestExprArrayDict exposing (..)
--import TestExprScorpedDefVar exposing (..)
import TestExprScorpedDefVarFunc exposing(..)



---------------------------------------------------------------------
type alias Name =
    String

type UserEnv
      = UserEnv
         { userFunctions : Dict.Dict String (List Expr, List Statement)
         , userEnv       : Dict.Dict String String
         }

userenvEmpty : UserEnv
userenvEmpty =
       UserEnv
         {
          userFunctions = Dict.empty
         ,userEnv = Dict.empty
         }

type Statement
    = Var Name
    | IfThen Expr (List Statement) 
    | IfThenElse Expr (List Statement) (List Statement)
    | IfThenElsIfThen Expr (List Statement) (List (Expr,(List Statement)))
    | IfThenElsIfThenElse Expr (List Statement) (List (Expr,(List Statement))) (List Statement)
    | Case        Expr  (List (Expr,(List Statement)))
    | While Expr (List Statement) 
    | For Expr Expr (List Statement) 
    | DefVar Expr Expr
    | DefFunc Expr (List Expr) (List Statement)
    | Assign Expr Expr
    | Return Expr 
    | Break
    | Continue
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
           , reserved = Set.fromList [ "if", "then", "else","elsif", "end", "while", "do","in", "for", "case", "default" ,"var" ,"def" , "return","break","continue"]
           }



typeVar : Parser Statement
typeVar =
     succeed Var
        |= variable
          { start = Char.isLower
          , inner = \c -> Char.isAlphaNum c || c == '_'
          --, reserved = Set.fromList [ "let", "in", "case", "default", "of" ]
          , reserved = Set.fromList [ "if", "then", "else","elsif","end", "while", "do","in", "for" , "case", "default" , "var", "def", "return","break","continue"]
          }


statement : Parser Statement
statement =
   oneOf
      [ defVarStatement
      , defFuncStatement
      , assignStatement
      , backtrackable ifStatement2  -- if then
      , backtrackable ifStatement   -- if then else
      , backtrackable ifStatement3  -- if then elsif else
      , backtrackable ifStatement4  -- if then elsif
      , backtrackable caseStatement -- case
      , whileStatement
      , forStatement
      , returnStatement
      ]


defVarStatement : Parser Statement 
defVarStatement =
  succeed DefVar
    |. spaces
    |. keyword "var"
    |. spaces
    |= expression
    |. spaces
    |. symbol "="
    |. spaces
    --|= typeVar
    |= expression
    |. spaces
    |. symbol ";"

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

returnStatement : Parser Statement 
returnStatement =
  succeed Return
    |. spaces
    |. keyword "return"
    |. spaces
    |= expression
    |. spaces
    |. symbol ";"

breakStatement : Parser Statement 
breakStatement =
  succeed Break
    |. spaces
    |. keyword "break"
    |. spaces
    |. symbol ";"

continueStatement : Parser Statement 
continueStatement =
  succeed Continue
    |. spaces
    |. keyword "continue"
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

defFuncStatement : Parser Statement 
defFuncStatement =
  succeed DefFunc 
    |. spaces
    |. keyword "def"
    |. spaces
    --|= lazy (\_ -> typeVar)
    |= lazy (\_ -> expression)
    |. symbol "("
    |. spaces
    |= formalArgValues
    |. spaces
    |. symbol ")"
    |. spaces
    |. symbol "do"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. symbol "end"
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

------------------------------------------------------------------

userFuncExec2_ : (List Expr) -> (List Statement) -> UserEnv -> (Array.Array ArgValue) -> OutVal
userFuncExec2_ args stmts userenv input_args =
            --OString "OK"
            let
              r = case (Array.get 0 input_args) of
                       Just (AvInt a)  -> 
                                   a
                       _ ->
                             0
            in
            --OString ( Debug.toString r)
            --OString ( Debug.toString input_args)
              --  AvInt 1 AvInt 2 AvInt 3
            OString ( Debug.toString args)
              -- Variavle a Variable b Variable c

userFuncExec2 : Context -> (List Expr) -> (List Statement) -> UserEnv -> (Array.Array ArgValue) -> OutVal
userFuncExec2 (Context base_context) args stmts userenv input_args =
            --OString "OK"
            let
              func index v =
                  let
                   name = case v of
                       Variable n -> 
                             n
                       _ ->
                             "__"

                   value  = case (Array.get index input_args) of
                       Just (AvInt a)  -> 
                                   OFloat (toFloat a)
                       Just (AvBool a)  -> 
                                   OBool a
                       Just (AvFloat a)  -> 
                                   OFloat a
                       Just (AvString a)  -> 
                                   OString a
                       _ ->
                             OFloat 0
                  in
                  (name, value)

              new_args = List.indexedMap func args
              context = empty

              func2 (name, v) context_ =
                        addConstant name v context_


              (Context context1) = List.foldl  func2 context new_args
              context2 =
                       Context
                          { context1
                              | functions = base_context.functions
                          }                          


              (userenv_2,ans) = eval2 userenv context2 stmts

              ret = getConstant "_return_" ans

{--
              ans2 = case ans of
                            Context a -> 
                                     a
--}
            in
            --OString ( Debug.toString r)
            --OString ( Debug.toString input_args)
              --  AvInt 1 AvInt 2 AvInt 3
            --OString ( Debug.toString args)
              -- Variavle a Variable b Variable c
            --OString ( Debug.toString new_args)
            --OString ( Debug.toString context2)
            --OString ( Debug.toString ans)

            case ret of
                Just r ->
                         r
                _ ->
                         OString ""

userFuncExec : UserEnv -> Context -> String -> (Array.Array ArgValue) -> OutVal
userFuncExec userenv context funcname input_args =
         --OString ("NOT FOUND:::" ++ funcname)
         let 
          result = userDefFuncGet funcname userenv context
         in
         case result of
               Ok (args, stmts) ->
                    let
                       r = userFuncExec2 context args stmts userenv input_args
                    in
                    --OString ("*FOUND::" ++ funcname)
                    r
               Err a ->
                    OString ("NOT FOUND:::" ++ funcname)

    


--exec_evaluate :  Context -> Expr -> OutVal
exec_evaluate :  UserEnv -> Context -> Expr ->  OutVal
exec_evaluate  (UserEnv userenv) context expr =       
     let
       r = evaluate  context expr
     in
    case r of
         ExprOk a ->
                a

         ExprNotFoundFunc (name, args) ->
                --OString ("NOT FOUND::" ++ a)
                userFuncExec (UserEnv userenv) context name args

         ExprErr a ->
                OString a


evalWhile : Expr -> (Array.Array Statement)  -> UserEnv -> Context -> (UserEnv,Context)
evalWhile expr arr  userenv context =
     let
        expr_ = exec_evaluate userenv context expr
     in
     case expr_ of
            OBool True ->  
                 let
                   (userenv_3,context_3) = evalStep  arr 0 userenv context
                 in
                 evalWhile expr arr  userenv_3 context_3

            OBool False -> 
                 (userenv, context)
            _ -> 
                 (userenv, context)


evalForHelp : String -> Array.Array OutVal ->  (Array.Array Statement)  -> UserEnv -> Context -> (UserEnv,Context)
evalForHelp name array stmt userenv context =
        let
          context_ =  case (Array.get 0 array) of
                             Just a ->
                               setConstant name  a context
                             _ ->
                               context

          userenv_context_3 = evalStep  stmt 0 userenv context_

          array2 = Array.slice 1 (Array.length array) array
        in
        if (Array.length array2) > 0 then
           --evalForHelp name array2 stmt userenv context_3
           evalForHelp name array2 stmt (Tuple.first userenv_context_3) (Tuple.second userenv_context_3)
        else
           userenv_context_3

evalFor : Expr -> Expr -> (Array.Array Statement)  -> UserEnv -> Context -> (UserEnv, Context)
evalFor val array stmt  userenv context =
     let
        --val_  = exec_evaluate context val
        val2 = case val of
                  --OString a ->
                  Variable a ->
                          a
                  _ ->
                          "not"

        array_ = exec_evaluate userenv context array
     in
     case array_ of
            OArray a_ ->  
                 evalForHelp val2 a_ stmt  userenv context

            _ -> 
                 (userenv, context)

evalElsIfs :  (List (Expr,(List Statement))) -> (List Statement) -> UserEnv -> Context -> (UserEnv,Context)
evalElsIfs   exprStmts  elseStmts  userenv context =
         let
             (expr, stmts ) = case (List.head exprStmts) of
                                     Just a -> 
                                              a
                                     _ ->
                                              (Bool False, [])
             a_ = exec_evaluate userenv context expr
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

             userenv_context_2 = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList stmts) 0 userenv context
                     OBool False -> -- else
                              let
                                new_exprStmts = List.drop 1 exprStmts
                              in
                              if (List.length new_exprStmts) > 0 then
                                 evalElsIfs   new_exprStmts  elseStmts userenv context

                              else
                                 evalStep (Array.fromList elseStmts) 0 userenv context

                     OString s ->
                              (userenv, context)
                     OFloat f ->
                              (userenv, context)
                     OArray _ ->
                              (userenv, context)
                     ODict _ ->
                              (userenv, context)
            in         
            --addConstant name a_ context_2 
            userenv_context_2 


evalElsIfs2 :  (List (Expr,(List Statement))) ->  UserEnv -> Context -> (UserEnv,Context)
evalElsIfs2   exprStmts  userenv  context =
         let
             (expr, stmts ) = case (List.head exprStmts) of
                                     Just a -> 
                                              a
                                     _ ->
                                              (Bool False, [])
             a_ = exec_evaluate userenv context expr
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

             userenv_context_2 = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList stmts) 0 userenv context
                     OBool False -> -- else
                              let
                                new_exprStmts = List.drop 1 exprStmts
                              in
                              if (List.length new_exprStmts) > 0 then
                                 evalElsIfs2   new_exprStmts  userenv context

                              else
                                 (userenv, context)

                     OString s ->
                             (userenv, context)
                     OFloat f ->
                             (userenv, context)
                     OArray _ ->
                             (userenv, context)
                     ODict _ ->
                             (userenv, context)
            in         
            --addConstant name a_ context_2 
            userenv_context_2 

evalCaseSwitch :  OutVal -> (List (Expr,(List Statement))) ->  UserEnv -> Context -> (UserEnv,Context)
evalCaseSwitch   target exprStmts  userenv context =
         let
             (expr, stmts ) = case (List.head exprStmts) of
                                     Just a -> 
                                              a
                                     _ ->
                                              (Bool False, [])
             a_ = exec_evaluate userenv context expr
             --cond = target == a_

             cond = case expr of 
                         Default a ->
                              True
                         _ ->
                              target == a_

             userenv_context_2 = case cond of
                      True ->  
                              evalStep (Array.fromList stmts) 0  userenv context
                      False -> 
                              let
                                new_exprStmts = List.drop 1 exprStmts
                              in
                              if (List.length new_exprStmts) > 0 then
                                 evalCaseSwitch target  new_exprStmts  userenv context

                              else
                                 (userenv, context)
            in         
            userenv_context_2 

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

--userDefFuncDic = Dict.empty

userDefFuncAdd : String -> (List Expr) -> (List Statement) -> UserEnv -> Context -> (UserEnv, Context)
userDefFuncAdd name argvs stmts (UserEnv userenv ) (Context context) =
      let
          log_ = context.log ++ "[f]" ++ name ++ " "
          userFunctions_= Dict.insert name (argvs, stmts)  userenv.userFunctions
      in
      (UserEnv
         { userenv
             | userFunctions = userFunctions_
         }
      ,Context
         { context
             | log = log_
         }
       )
      
userDefFuncGet : String -> UserEnv -> Context 
     -> Result String  (List Expr , List Statement)
userDefFuncGet name  (UserEnv userenv ) (Context context) =
      let
          log_ = context.log ++ "[f]" ++ name ++ " "
          -- userFunctions_= Dict.insert name (argvs, stmts)  userenv.userFunctions
          --(argvs, stms) = userFunctions_= Dict.get name   userenv.userFunctions
          result =  Dict.get name   userenv.userFunctions

      in
      case result of
          Just a ->
                Ok a
          _ ->
                Err ("..NOT FOUND:" ++ name)


evalStep : (Array.Array Statement) -> Int -> UserEnv -> Context -> (UserEnv, Context)
evalStep arr pos userenv context =
    let
      _ = Debug.log (Debug.toString pos)
      userenv_context_pair = case (Array.get pos arr) of
        Just (DefVar a b) ->
            let
               b_ = exec_evaluate userenv context b
               name = case a of
                     Variable n ->
                               n
                     _ ->
                               "undef"
            in         
            (userenv, newConstant name b_ context  )

        Just (DefFunc a b c) ->
            let
               name = case a of
                     Variable n ->
                               n
                     _ ->
                               "undef"
            in         
            -- newConstant name b_ context  
            -- context
            userDefFuncAdd name b c userenv context

        Just (Assign a b) ->
            let
               b_ = exec_evaluate userenv context b
               name = case a of
                     Variable n ->
                               n
                     _ ->
                               "undef"
            in         
            (userenv, (setConstant name b_  context)  )

        Just (Return a ) ->
            let
               a_ = exec_evaluate userenv context a
            in         
            (userenv, (addConstant "_return_" a_  context)  )

        Just (IfThenElse a b c) ->
            let
               a_ = exec_evaluate userenv context a
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
               (userenv_2,context_2) = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList b) 0 userenv context_1
                     OBool False -> -- else
                              evalStep (Array.fromList c) 0 userenv context_1
                     OString s ->
                              (userenv, context_1)
                     OFloat f ->
                              (userenv, context_1)
                     OArray _ ->
                              (userenv, context_1)
                     ODict _ ->
                              (userenv, context_1)


               context_3 = scopePop context_2
            in         
            (userenv_2, context_3 )


        Just (IfThen a b ) ->
            let
               a_ = exec_evaluate userenv context a
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
               (userenv_2,context_2) = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList b) 0 userenv context_1
                     OBool False -> -- else
                              (userenv, context_1)
                     OString s ->
                              (userenv, context_1)
                     OFloat f ->
                              (userenv, context_1)
                     OArray _ ->
                              (userenv, context_1)
                     ODict _ ->
                              (userenv, context_1)
               context_3 = scopePop context_2
            in         
            (userenv_2, context_3 )

        Just (IfThenElsIfThenElse a b c d) ->
            let
               a_ = exec_evaluate userenv context a
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
               (userenv_2,context_2) = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList b) 0 userenv context_1
                     OBool False -> -- else
                              evalElsIfs c d userenv context_1
                     OString s ->
                              (userenv, context_1)
                     OFloat f ->
                              (userenv, context_1)
                     OArray _ ->
                              (userenv, context_1)
                     ODict _ ->
                              (userenv, context_1)
               context_3 = scopePop context_2
            in         
            (userenv_2, context_3 )

        Just (IfThenElsIfThen a b c ) ->
            let
               a_ = exec_evaluate userenv context a
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
               (userenv_2,context_2) = case a_ of
                     OBool True ->  -- then
                              evalStep (Array.fromList b) 0 userenv context_1
                     OBool False -> -- else
                              evalElsIfs2 c  userenv context_1
                     OString s ->
                              (userenv,context_1)
                     OFloat f ->
                              (userenv,context_1)
                     OArray _ ->
                              (userenv,context_1)
                     ODict _ ->
                              (userenv,context_1)
               context_3 = scopePop context_2
            in         
            (userenv_2, context_3 )

        Just (Case a b  ) ->
            let
               a_ = exec_evaluate userenv context a
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
               (userenv_2,context_2) = evalCaseSwitch a_ b  userenv context_1
               context_3 = scopePop context_2

            in         
            (userenv_2, context_3 )

        Just (While a b ) ->
            let
               context_1 = scopePush context
               (userenv_2,context_2) = evalWhile a (Array.fromList b) userenv context_1
               context_3 = scopePop context_2
            in         
            (userenv_2, context_3 )


        Just (For a b c) ->
            let
               context_1 = scopePush context
               (userenv_2,context_2) = evalFor a b (Array.fromList c) userenv context_1
               context_3 = scopePop context_2
            in         
            (userenv_2, context_3 )

        _ ->
            (userenv,context)
    in
    if Array.length arr <= pos then
       --(userenv, context_)
       userenv_context_pair

    else
       --(userenv, evalStep arr (pos + 1) userenv, context_)
       evalStep arr (pos + 1) (Tuple.first userenv_context_pair) (Tuple.second userenv_context_pair)

----------------------------------------------------------
parse script_name =
   run script script_name

eval1 : UserEnv -> Context -> List Statement -> OutVal
eval1 userenv context stmts =
    let
      _ = Debug.log (Debug.toString stmts)
      arr = Array.fromList stmts
      userenv_context_ = evalStep arr 0 userenv context
    in
      OString (Debug.toString userenv_context_)

exec1 script_name =
  let
     ast = run script script_name
     result = case ast of
          Err err ->
              Debug.toString err
          Ok  stmts ->
              let
                 context = empty
                 userenv = userenvEmpty
                 ans = eval1 userenv context stmts
              in
              Debug.toString ans
  in
  result
   
--------------------------------------------------
eval2 : UserEnv -> Context -> List Statement -> (UserEnv, Context)
eval2 userenv context stmts =
      --OString "EVAL"
    let
      _ = Debug.log (Debug.toString stmts)
      arr = Array.fromList stmts
      userenv_context_ = evalStep arr 0 userenv context
    in
      userenv_context_

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

                 userenv = userenvEmpty
                 (userenv_2,ans) = eval2 userenv context stmts

                 ans2 = case ans of
                            Context a -> 
                                     a

                 env = Debug.toString  userenv_2

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
              ans4 ++ " -> " ++ log ++ "[" ++ env
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
script2 = """
  var a = 1;
  var b = 0;
  var c = 0;

  if a > 1 then
     b = 1;
  else
     b = 2;
     c = 1;
  end
   a = 100;

  c = strjoin("ABC", "abc");

"""
script3 = """
  var a = 1;
  var b = 0;
  var c = 0;


  def test ( a, b, c) do
     a = a + b + c;

     return a;

  end

  def str ( l, r ) do
     return strjoin(l, r) ;
  end

  if a > 1 then
     b = 1;
  else
     var b = 2;
     c = 1;
  end
   a = 100;

  c = strjoin("ABC", "abc"); //lib

  var e1 = "abc";
  var e2 = "ABC";

  var e = strjoin(e1, e2); //lib

  var d = test(1,2,3);       //user func

  var ss = str("xyz", "1XYZ");

  return a;

"""
script4 = """
   var total = 0;

   var ok = 0;

   for ok in [1,2,3,4,5,6,7,8,9] do

    total = total + ok;

   end

"""
script5 = """
   var total = 0;

   var ok = 0;

   while total < 45 do

    total = total + 1;

   end

"""

help = """
parse input3
exec1 input3
exed2 input3
"""
