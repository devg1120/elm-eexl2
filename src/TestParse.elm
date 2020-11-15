module TestParse exposing (..)


import Parser exposing (..)
import Char
import Set

{--
 run : Parser a -> String -> Result (List DeadEnd) a

--}


---------------------------------------------------------------------
type alias Name =
    String

type Statement
    = Var Name
    | If Statement (List Statement) (List Statement)
    | While Statement (List Statement) 
    | For Statement Statement (List Statement) 
    | Cmd Statement Statement
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

statementsHelp : List Statement -> Parser (Step (List Statement) (List Statement))
statementsHelp revStmts =
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

typeCommand : Parser Statement
typeCommand =
     succeed Var
        |= variable
           { start = Char.isUpper
           , inner = \c -> Char.isAlphaNum c || c == '_'
           , reserved = Set.fromList [ "let", "in", "case", "of" ]
           }



typeVar : Parser Statement
typeVar =
     succeed Var
        |= variable
          { start = Char.isLower
          , inner = \c -> Char.isAlphaNum c || c == '_'
          , reserved = Set.fromList [ "let", "in", "case", "of" ]
          }


statement : Parser Statement
statement =
   oneOf
      [ commandStatement
      , ifStatement
      , whileStatement
      , forStatement
      ]


commandStatement : Parser Statement 
commandStatement =
  succeed Cmd
    |. spaces
    |= typeCommand
    |. spaces
    |. symbol ","
    |. spaces
    |= typeVar
    |. spaces

ifStatement : Parser Statement 
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

whileStatement : Parser Statement 
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
   
   while test1 do
   
      Ddd,a;
      Ddd,a;
   end;

   for test in range do
   
      Str, zzzz;
      Ddd,a;
      Ddd,a;
   end;

   for test in range do
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

-- run : Result (List Parser.DeadEnd) (List Statement)
r005 = run script input2

input3 = """
   Aaa,a12
   Baa,b12
   Baa,b12

   if ccc then
      Ccc,ddd
      Ccc,ddd

   else 
      Ddd,eee
      Ddd,eee
   end
   
   while test1 do
   
      Ddd,a
      Ddd,a
   end

   for test in range do
   
      Str, zzzz
      Ddd,a
      Ddd,a
   end

   for test in range do
      Ddd,a
      Ddd,a
      if ccc then
         Ccc,ddd
         Ccc,ddd

      else 
         Ddd,eee
         Ddd,eee
      end
   end
"""

r006 = run script input3
