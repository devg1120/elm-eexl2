module TestParse exposing (..)


import Parser exposing (..)
import Char
import Set

{--
 run : Parser a -> String -> Result (List DeadEnd) a

--}

r001 = run int "123456" 

-----------------------------------------------------------
{--
typeVar : Parser String
typeVar =
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList [ "let", "in", "case", "of" ]
    }


r002 = run typeVar "te_001 "
--}
-----------------------------------------------------------
{--
type alias Point = { x : Float, y : Float }

point : Parser Point
point =
  succeed Point
    |. symbol "("
    |. spaces
    |= float
    |. spaces
    |. symbol ","
    |. spaces
    |= float
    |. spaces
    |. symbol ")"

r003 = run point "( 2,3)"
--}
-----------------------------------------------------------
{--
script_t : Parser String
script_t =
  succeed (++)
    |. symbol "("
    |. spaces
    |= typeVar
    |. spaces
    |. symbol ","
    |. spaces
    |= typeVar
    |. spaces
    |. symbol ")"


r004 = run script_t "( abc,xyz_)"
--}
-----------------------------------------------------------
{--
script : Parser (List String)
script =
   commands
commands : Parser String
commands =
    loop "" commandsHelp
     
commandsHelp : String -> Parser (Step String String)
commandsHelp str =
    oneOf
      [ succeed (\stmt -> Loop (stmt ++ str))
          |= command
      , succeed ()
          --|> map (\_ -> Done (String.reverse str))
          |> map (\_ -> Done ("--" ++ str ++ "--"))
      ]
--}
{--
script : Parser (List String)
script =
   commands
commands : Parser (List String)
commands =
    loop [] commandsHelp
     
commandsHelp : List String -> Parser (Step (List String) (List String))
commandsHelp revStmts =
    oneOf
      [ succeed (\stmt -> Loop (stmt :: revStmts))
          |= command
      , succeed ()
          |> map (\_ -> Done (List.reverse revStmts))
          --|> map (\_ -> Done ("--" ++ str ++ "--"))
      ]

typeCommand : Parser String
typeCommand =
  variable
    { start = Char.isUpper
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList [ "let", "in", "case", "of" ]
    }

typeVar : Parser String
typeVar =
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList [ "let", "in", "case", "of" ]
    }

command : Parser String
command =
  --succeed (++)
  succeed Tuple.pair
    |. spaces
    |. symbol "["
    |. spaces
    |= typeCommand
    |. spaces
    |. symbol ","
    |. spaces
    |= typeVar
    |. spaces
    |. symbol "]"
    |. spaces
    |> andThen
        (\(cmd,val) ->
             succeed (cmd ++ ":" ++ val)
             )


input = """
   [Aaa,a12]
[Baa,b12]
[Baa,b12]
"""

r005 = run script input

--}

{--
type alias Name =
    String

type Expr
    = Var Name
    | If Expr Expr Expr
    | Cmd Expr Expr

script : Parser (List String)
script =
   statements
statements : Parser (List String)
statements =
    loop [] statementsHelp
     
statementsHelp : List String -> Parser (Step (List String) (List String))
statementsHelp revStmts =
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

typeCommand : Parser String
typeCommand =
  variable
    { start = Char.isUpper
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList [ "let", "in", "case", "of" ]
    }

typeVar : Parser String
typeVar =
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList [ "let", "in", "case", "of" ]
    }


statement : Parser String
statement =
   oneOf
      [ commandStatement
      , ifStatement
      ]

commandStatement : Parser String
commandStatement =
  --succeed (++)
  succeed Tuple.pair
    |. spaces
    |= typeCommand
    |. spaces
    |. symbol ","
    |. spaces
    |= typeVar
    |. spaces
    |> andThen
        (\(cmd,val) ->
             succeed (cmd ++ ":" ++ val)
             )

ifStatement : Parser String
ifStatement =
  --succeed (++)
  succeed  Tuple.pair
    |. spaces
    |= keyword "if"
    |. spaces
    |= typeVar
    |. spaces
    |> andThen
        (\(cmd, arg) ->
             succeed ("IF" ++ ":" ++ arg)
             )
input = """
   Aaa,a12;
Baa,b12;
Baa,b12;
if ccc;
"""

r005 = run script input
--}

type alias Name =
    String

type Expr
    = Var Name
    --| If Expr Expr Expr
    | If Expr (List Expr) (List Expr)
    | While Expr (List Expr) 
    | For Expr Expr (List Expr) 
    | Cmd Expr Expr

script : Parser (List Expr)
script =
   statements
statements : Parser (List Expr)
statements =
    loop [] statementsHelp
     
statementsHelp : List Expr -> Parser (Step (List Expr) (List Expr))
statementsHelp revStmts =
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

typeCommand : Parser Expr
typeCommand =
     succeed Var
        |= variable
           { start = Char.isUpper
           , inner = \c -> Char.isAlphaNum c || c == '_'
           , reserved = Set.fromList [ "let", "in", "case", "of" ]
           }



typeVar : Parser Expr
typeVar =
     succeed Var
        |= variable
          { start = Char.isLower
          , inner = \c -> Char.isAlphaNum c || c == '_'
          , reserved = Set.fromList [ "let", "in", "case", "of" ]
          }


statement : Parser Expr
statement =
   oneOf
      [ commandStatement
      , ifStatement
      , whileStatement
      ]

commandStatement : Parser Expr 
commandStatement =
  succeed Cmd
    |. spaces
    |= typeCommand
    |. spaces
    |. symbol ","
    |. spaces
    |= typeVar
    |. spaces

ifStatement : Parser Expr 
ifStatement =
  succeed  If
    |. spaces
    |. keyword "if"
    |. spaces
    |= lazy (\_ -> typeVar)
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

whileStatement : Parser Expr 
whileStatement =
  succeed  While
    |. spaces
    |. keyword "while"
    |. spaces
    |= lazy (\_ -> typeVar)
    |. spaces
    |. symbol "do"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. symbol "end"
    |. spaces


forStatement : Parser Expr 
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
input2 = """
   Aaa,a12;
   Baa,b12;
   Baa,b12;

   if ccc then
      Ccc,ddd;
      Ccc,ddd;

   else 
      Ddd,eee;
      Ddd,eee;
   end;
   
   while test do
   
      Ddd,a;
      Ddd,a;
   end;

   for test in do
   
      Ddd,a;
      Ddd,a;
   end;

   for test in do
   
      Ddd,a;
      Ddd,a;
      if ccc then
         Ccc,ddd;
         Ccc,ddd;

      else 
         Ddd,eee;
         Ddd,eee;
      end;
   end;
"""

r005 = run script input2
