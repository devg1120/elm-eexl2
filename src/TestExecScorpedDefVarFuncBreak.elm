--module TestExecScorpedDefVarFunc exposing (..)
module TestExecScorpedDefVarFuncBreak exposing (..)


import Parser exposing (..)
import Char
import Array
import Dict
import Set

--import TestExpr exposing (..)
--import TestExprArrayDict exposing (..)
--import TestExprScorpedDefVar exposing (..)
--import TestExprScorpedDefVarFunc exposing(..)
import TestExprScorpedDefVarFuncBreak exposing(..)



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
           , reserved = Set.fromList [ "if", "then", "else","elsif", "end", "while", "do","in", "for", "case", "default" ,"let" ,"fn" , "return","break","continue"]
           }



typeVar : Parser Statement
typeVar =
     succeed Var
        |= variable
          { start = Char.isLower
          , inner = \c -> Char.isAlphaNum c || c == '_'
          --, reserved = Set.fromList [ "let", "in", "case", "default", "of" ]
          , reserved = Set.fromList [ "if", "then", "else","elsif","end", "while", "do","in", "for" , "case", "default" , "let", "fn", "return","break","continue"]
          }


statement : Parser Statement
statement =
   oneOf
      [ defVarStatement
      , defFuncStatement
      , assignStatement
      --, backtrackable ifStatement2  -- if then
      , backtrackable ifStatement   -- if then else
      , backtrackable ifStatement3  -- if then elsif else
      , backtrackable ifStatement4  -- if then elsif
      , backtrackable ifStatement2  -- if then
      , backtrackable caseStatement -- case
      , whileStatement
      , forStatement
      , returnStatement
      , breakStatement
      , continueStatement
      ]


defVarStatement : Parser Statement 
defVarStatement =
  succeed DefVar
    |. spaces
    |. keyword "let"
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
    |. keyword "{"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "}"
    |. spaces
    |. keyword "else"
    |. spaces
    |. keyword "{"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "}"
    |. spaces

ifStatement2 : Parser Statement 
ifStatement2 =
  succeed  IfThen
    |. spaces
    |. keyword "if"
    |. spaces
    |= lazy (\_ -> expression)
    |. spaces
    |. keyword "{"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "}"
    |. spaces


elsIfBlock :  Parser (Expr,(List Statement))
elsIfBlock  =
  succeed Tuple.pair
    |. spaces
    |. keyword "elsif"
    |. spaces
    |= lazy (\_ -> expression)
    |. spaces
    --|. keyword "then"
    |. keyword "{"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "}"
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
    --|. keyword "then"
    |. keyword "{"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "}"
    |. spaces
    |= lazy (\_ -> elsIfBlocks)
    --|. spaces
    --|. keyword "}"
    |. spaces
    |. keyword "else"
    |. spaces
    |. keyword "{"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    --|. keyword "end"
    |. keyword "}"
    |. spaces

ifStatement4 : Parser Statement 
ifStatement4 =
  succeed  IfThenElsIfThen
    |. spaces
    |. keyword "if"
    |. spaces
    |= lazy (\_ -> expression)
    |. spaces
    --|. keyword "then"
    |. keyword "{"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "}"
    |. spaces
    |= lazy (\_ -> elsIfBlocks)
    |. spaces
    --|. keyword "end"
    --|. keyword "}"
    |. spaces

caseStatement : Parser Statement 
caseStatement =
  succeed  Case
    |. spaces
    |. keyword "case"
    |. spaces
    |= lazy (\_ -> expression)
    |. spaces
    |. keyword "{"
    |. spaces
    |= lazy (\_ -> caseBlocks)
    |. spaces
    |. keyword "}"
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
    |. keyword "fn"
    |. spaces
    --|= lazy (\_ -> typeVar)
    --|= lazy (\_ -> expression)
    --|= expression
    |= typevar  -- func name
    |. spaces
    |. symbol "("
    |. spaces
    |= formalArgValues
    |. spaces
    |. symbol ")"
    |. spaces
    |. symbol "{"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. symbol "}"
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
    |. symbol "{"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. symbol "}"
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
    |. keyword "{"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "}"
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
                       Just (AvVar a)  -> 
                                   let
                                      --value_ = getConstant a context
                                      value_ = getConstant a (Context base_context)
                                   in
                                   case value_ of
                                          Just v_ ->
                                                 v_
                                                 --OFloat 3
                                          _ ->
                                                 (OString " AvVar not_found") 
                       _ ->
                             OFloat 3

                  in
                  (name, value)

              new_args = List.indexedMap func args
              context = empty
              context__ = setFuncTop True context

              func2 (name, v) context_ =
                        addConstant name v context_


              (Context context1) = List.foldl  func2 context__ new_args
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
                         OString "NG"

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
       --r = evaluate  context expr
       r = evaluate  (UserEnv userenv) userFuncExec context expr
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
        expr_  = exec_evaluate userenv context expr
     in
     case expr_ of
            --OBool True ->  
            --     let
            --       (userenv_3,context_3) = evalStep  arr 0 userenv context
            --     in
            --     evalWhile expr arr  userenv_3 context_3

            OBool True ->  
                 let
                   (userenv_3,context_3) = evalStep  arr 0 userenv context
                 in
                 if isBreak context_3 then
                   --(userenv_3, context_3)
                   let
                     context_4 = setBreak False context_3
                   in
                   (userenv_3, context_4)
                 else
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
           --evalForHelp name array2 stmt (Tuple.first userenv_context_3) (Tuple.second userenv_context_3)
           let
            context_3 = Tuple.second userenv_context_3
            userenv_3 = Tuple.first  userenv_context_3
           in
           if isBreak context_3 then
             let
               context_4 = setBreak False context_3
             in
             (userenv_3, context_4)
           else
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

-----------------------------------
isReturn : Context -> Bool
isReturn context =
           let
            context_record = case context of
                        Context con -> 
                                 con
           in
           context_record.return

setReturn : Bool -> Context -> Context
setReturn bool context =
           let
            context_record = case context of
                        Context con -> 
                                 con
           in
            Context
                 {
                 context_record
                      | return  = bool
                 }

isFuncTop : Context -> Bool
isFuncTop context =
           let
            context_record = case context of
                        Context con -> 
                                 con
           in
           context_record.functop

setFuncTop : Bool -> Context -> Context
setFuncTop bool context =
           let
            context_record = case context of
                        Context con -> 
                                 con
           in
            Context
                 {
                 context_record
                      | functop  = bool
                 }

isBreak : Context -> Bool
isBreak context =
           let
            context_record = case context of
                        Context con -> 
                                 con
           in
           context_record.break

setBreak : Bool -> Context -> Context
setBreak bool context =
           let
            context_record = case context of
                        Context con -> 
                                 con
           in
            Context
                 {
                 context_record
                      | break  = bool
                 }

isContinue : Context -> Bool
isContinue context =
           let
            context_record = case context of
                        Context con -> 
                                 con
           in
           context_record.continue

setContinue bool context =
           let
            context_record = case context of
                        Context con -> 
                                 con
           in
            Context
                 {
                 context_record
                      | continue  = bool
                 }

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

copyReturn : Context -> Context -> Context
copyReturn top old =
           let
               ret = getConstant "_return_" old

           in
           case ret of
                  Just r ->
                        addConstant "_return_" r top
                  _ ->
                        top

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
                     new_context =  Context
                               {
                                context_record
                                 | constants  = ct
                                 }

                   in
                   copyReturn new_context context
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
               context_ = setReturn True context
            in         
            (userenv, (addConstant "_return_" a_  context_)  )

        Just (Break  ) ->
            let
               context_ = setBreak True context 
            in         
            (userenv,  context_ )

        Just (Continue  ) ->
            let
               context_ = setContinue True context 
            in         
            (userenv,  context_ )

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
       userenv_context_pair

    else
       --evalStep arr (pos + 1) (Tuple.first userenv_context_pair) (Tuple.second userenv_context_pair)
       let
         context__ = Tuple.second userenv_context_pair
       in
       if isBreak context__ || isReturn context__ then
          userenv_context_pair
       else
          if isContinue context__ then
             userenv_context_pair
          else
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
userFuncReg : UserEnv -> Context -> List Statement -> (UserEnv, Context)
userFuncReg userenv context stmts =
      --OString "EVAL"
    let
      _ = Debug.log (Debug.toString stmts)
      arr = Array.fromList stmts
      userenv_context_ = evalStep arr 0 userenv context
    in
      userenv_context_

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
                 --(userenv_ ,_  ) = userFuncReg userenv context stmts
                 --(userenv_2,ans) = eval2 userenv_ context stmts
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
                 
                 --ans4 = Debug.toString  (ans3)
                 ans4 = Debug.toString  (ans2)
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
   let total = 0;


   let aaa = 100;
   let bbb = 200;
   let ccc = 300;
   let aa = 0;
   let bb = 0;
   let cc = 0;


   if aaa < bbb{
      aa = 1;
   } else {
      bb=  1;
   }

   if aaa < bbb {
      aa = aa + 2;
   }


   let za = 100;
   let zb = 200;
   let zc = 300;
   let zd = 400;

   let z = 0;
/*
   if za > zb {
      z = 1;
    } elsif  za > zc {
      z=  2;
    } elsif  za > zd {
      z=  3;
    } else {
      z=  4;
   }


   let xa = 350;
   let xb = 200;
   let xc = 300;
   let xd = 400;

   let x = 0;

   if xa < xb {
      x = 1;
    } elsif  xa < xc {
      x=  2;
    } elsif  xa < xd {
      x=  3;
   }
*/
   let ca = 6;
   let re = 0;
   case ca {
       0:
          re = 1;
       1:
          re = 2;
       2:
          re = 3;
   
       default:
        re = 5;
   }


"""

input5 = """


   let za = 150;
   let zb = 400;
   let zc = 300;
   let zd = 200;

   let z = 0;

   if za > zb {
      z = 1;
    } elsif  za > zc {
      z=  2;
    } elsif  za > zd {
      z=  3;
    } else {
      z=  4;
   }

"""
input6 = """


   let za = 350;
   let zb = 400;
   let zc = 300;
   let zd = 200;

   let z = 0;

   if za > zb {
      z = 1;
    } elsif  za > zc {
      z=  2;
    } elsif  za > zd {
      z=  3;
   }

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
  let a = 1;
  let b = 0;
  let c = 0;


  fn test ( a, b, c) {
     a = a + b + c;

     return a;

  }

  fn str ( l, r ) {
     return strjoin(l, r) ;
  }

  if a > 1 {
     b = 1;
  } else {
     let b = 2;
     c = 1;
  }
   a = 100;

  c = strjoin("ABC", "abc"); //lib

  let e1 = "abc";
  let e2 = "ABC";

  let e = strjoin(e1, e2); //lib

  let d = test(1,2,3);       //user func

  let ss = str("xyz", "1XYZ");

  return a;

"""
script4 = """
   let total = 0;

   let ok = 0;

   for ok in [1,2,3,4,5,6,7,8,9] {

    total = total + ok;

   }

"""
script5 = """
   let total = 0;

   let ok = 0;

   while total < 45 {

    //total = total + 1;

    if total > 10 {
        break;
    }
    total = total + 1;

   }

   //break;

   //continue;

"""
script6 = """
   let total = 0;

   let ok = 0;

   for ok in [1,2,3,4,5,6,7,8,9] {

    total = total + ok;
    if total > 10 {
        break;
    }

   }

"""
script7 = """
  let a = 1;
  let b = 0;
  let c = 0;


  fn test ( a, b, c) {
     a = a + b + c;

     if a > 1 {
        a = 0;
     }

     return a;

  }

  fn test2 (a) {

     if a > 1 {
        a = 100;
     }

     return a;

  }

  fn str( l, r ) {
     return strjoin(l, r) ;
  }

  if a > 1 {
     b = 1;
  } else {
     let b = 2;
     c = 1;
  }
   a = 100;

  c = strjoin("ABC", "abc"); //lib

  let e1 = "abc";
  let e2 = "ABC";

  let e = strjoin(e1, e2); //lib

  let d = test(1,2,3);       //user func
  let d2 = test2(2);       //user func

  let ss = str("xyz", "1XYZ");


"""

fib = """

  fn fib(n) {
    
    let a = 0;

    if n < 2 {
      a =  n;
    } else {
      //a = fib(n-1) + fib(n-2); //式 引数
      let n1 = n -1;
      let n2 = n -2;

      let a1 = fib(n1) ;
      let a2 = fib(n2) ;
      a = a1 + a2;
    }
    return a;
  }


let result = fib(13);


"""

fib2 = """

  fn fib (n) {
    
    let a = 0;

    if n < 2 {
      a =  n;
    } else {
      //a = fib(n-1) + fib(n-2); //式 引数
      let n1 = n -1;
      let n2 = n -2;

      a = fib(n1) + fib(n2);
    }
    return a;
  }


let result = fib(13);


"""
fib3 = """

  fn fib(n) {

    if n < 2 {
      return n;
    } else {
      //a = fib(n-1) + fib(n-2); //式 引数
      let n1 = n -1;
      let n2 = n -2;

      let x = fib(n1) + fib(n2);
      return x;
    }
  }


let result = fib(13);


"""
fib4 = """

  fn fib(n) {

    if n < 2 {
      return n;
    } else {
      //a = fib(n-1) + fib(n-2); //式 引数
      let n1 = n -1;
      let n2 = n -2;

      return fib(n1) + fib(n2);
    }
  }


let result = fib(13);


"""
fib5 = """

  fn fib(n) {

    if n < 2 {
      return n;
    } else {
      //a = fib(n-1) + fib(n-2); //式 引数

      return fib(n-1) + fib(n-2);
    }
  }


let result = fib(13);


"""
fib6 = """

  fn sum(n) {
    return n + 3;
  }
  fn sumstr(n) {
    return  n + "3";
  }

  fn fib(n) {
    return sum(n);
  }

let  result1 = fib(9);
let  result2 = sum(9);
let  z = 9;
let  result3 = sum(z);
let  zs = "9";
let  result4 = sumstr(zs);
let  result5 = sum(z) + sum(z);
let  result6 = 1 + 2 ;
let  result7 = 1 + z ;
let  result8 = strjoin("A","B") + strjoin("C","D") ;

let e1 = "abc";
let e2 = "def";
let e = strjoin(e1, e2); //lib

"""


{--

https://github.com/jonathandturner/rhai/blob/master/scripts/fibonacci.rhai#L16

const target = 30;

fn fib(n) {
    if n < 2 {
         n
    } else {
        fib(n-1) + fib(n-2)
    }
}


let result = fib(target);


--}

help = """
parse input3
exec1 input3
exed2 input3
"""
