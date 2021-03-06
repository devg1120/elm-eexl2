module TestExpr exposing (..)

import Parser exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)
import Array



-- EXPRESSIONS


type Expr
  = Integer Int
  | Floating Float
  | String String
  | Bool Bool
  --| Variable OutVal
  | Variable String
  | Function String (Array.Array ArgValue)
  | Add Expr Expr   --[+]  String Float
  | Sub Expr Expr   --[-]  Float
  | Mul Expr Expr   --[*]  Float
  | Div Expr Expr   --[/]  Float
  | Div2 Expr Expr  --[//] Float
  | Div3 Expr Expr  --[%]  Float
  | And  Expr Expr  --[&&] Bool
  | Or   Expr Expr  --[||] Bool
  | LT   Expr Expr  --[<]  Bool
  | GT   Expr Expr  --[>]  Bool
  | LE   Expr Expr  --[<=] Bool
  | GE   Expr Expr  --[>=] Bool
  | EQ   Expr Expr  --[==] Bool
  | NE   Expr Expr  --[!=] Bool

type OutVal
  = OFloat  Float
  | OString String
  | OBool Bool


--------------------------------------------------------- context

type Context
    = Context
        { constants : Dict String OutVal
        , functions : Dict String (Context -> Input -> OutVal)
        }

empty : Context
empty =
    Context
        { constants = Dict.empty
        , functions = Dict.empty
        }

addConstant : String -> OutVal -> Context -> Context
addConstant name value (Context context) =
    Context
        { context
            | constants = context.constants |> Dict.insert name value
        }


--addFunction : String -> (Input -> Maybe OutVal) -> Context -> Context
addFunction : String -> (Context -> Input -> OutVal) -> Context -> Context
addFunction name f (Context context) =
    Context
        { context
            | functions = context.functions |> Dict.insert name f
        }


getConstant : String -> Context -> Maybe OutVal
getConstant name (Context { constants }) =
    Dict.get name constants


getFunction : String -> Context -> Maybe (Context -> Input -> OutVal)
getFunction name (Context { functions }) =
    Dict.get name functions


--------------------------------------------------------- evalate

--evaluate : Expr -> OutVal
--evaluate  expr =
evaluate : Context -> Expr -> OutVal
evaluate context expr =
  case expr of
    Variable name ->
       let
          --value = OString "testOK"
          value = getConstant name context
          result = case value of
                      Just v ->
                             v
                      _ ->
                             (OString "not_found")
       in
       result
     
    Function name args ->
         --  getFunction name context
         --       |> Maybe.map (\fn -> succeed <| fn args)
         --       |> Maybe.withDefault (problem <| "Unknown function '" ++ name ++ "'")
         let
           func_ = getFunction name context
           ans = case func_ of
                     Just f ->
                            (f context args)
                     _ ->
                            (OString " !!not_found")
         in
         ans


    String s ->
     OString  s

    Integer n ->
     OFloat (toFloat n)

    Floating n ->
     OFloat  n

    Bool n ->
     OBool  n

    Add a b ->
    {--
     let
       a_ = case (evaluate a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate b) of
               OFloat n -> n
               _ -> 0
     in
     OFloat ( a_ +  b_)
    --}
     let
       a_ = evaluate context a
       b_ = evaluate context b
     in
     case (a_, b_) of
        (OFloat aa,  OFloat bb) ->
                OFloat  ( aa + bb )

        (OString aa, OString bb) ->
                OString ( aa ++ bb )
        _ ->
                OFloat 0

    Sub a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
     OFloat ( a_ -  b_)

    Mul a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
     OFloat ( a_ *  b_)

    Div a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
     OFloat ( a_ / b_)

    Div2 a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
     OFloat ((toFloat ((floor a_) // (floor  b_))))

    Div3 a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
      let
        a2 = floor  a_ 
        b2 = floor  b_ 

        div_ = a2 // b2
        ans_ = a2 - (div_ * b2)

      in
      OFloat (toFloat ans_)

    And a b ->
     let
       a_ = case (evaluate context a) of
               OBool n -> n
               _ -> False
       b_ = case (evaluate context b) of
               OBool n -> n
               _ -> False
     in
     OBool ( a_ && b_)

    Or a b ->
     let
       a_ = case (evaluate context a) of
               OBool n -> n
               _ -> False
       b_ = case (evaluate context b) of
               OBool n -> n
               _ -> False
     in
     OBool ( a_ || b_)

    -----------------------
    LT a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
     OBool ( a_ < b_)

    GT a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
     OBool ( a_ > b_)

    LE a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
     OBool ( a_ <= b_)

    GE a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
     OBool ( a_ >= b_)

    EQ a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
     OBool ( a_ == b_)

    NE a b ->
     let
       a_ = case (evaluate context a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate context b) of
               OFloat n -> n
               _ -> 0
     in
     OBool ( a_ /= b_)

parse : String -> Result (List DeadEnd) Expr
parse string__ =
  run expression string__


-- STRINGS


--string : Parser String
string : Parser Expr
string =
  --succeed (String identity)
  succeed (\identity -> String identity)
    |. token "\""
    |= loop [] stringHelp


stringHelp : List String -> Parser (Step (List String) String)
stringHelp revChunks =
  oneOf
    [ succeed (\chunk -> Loop (chunk :: revChunks))
        |. token "\\"
        |= oneOf
            [ map (\_ -> "\n") (token "n")
            , map (\_ -> "\t") (token "t")
            , map (\_ -> "\r") (token "r")
            , succeed String.fromChar
                |. token "u{"
                |= unicode
                |. token "}"
            ]
    , token "\""
        |> map (\_ -> Done (String.join "" (List.reverse revChunks)))
    , chompWhile isUninteresting
        |> getChompedString
        |> map (\chunk -> Loop (chunk :: revChunks))
    ]


isUninteresting : Char -> Bool
isUninteresting char =
  char /= '\\' && char /= '"'



-- UNICODE


unicode : Parser Char
unicode =
  getChompedString (chompWhile Char.isHexDigit)
    |> andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
  let
    length = String.length str
    code = String.foldl addHex 0 str
  in
  if 4 <= length && length <= 6 then
    problem "code point must have between 4 and 6 digits"
  else if 0 <= code && code <= 0x10FFFF then
    succeed (Char.fromCode code)
  else
    problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
  let
    code = Char.toCode char
  in
  if 0x30 <= code && code <= 0x39 then
    16 * total + (code - 0x30)
  else if 0x41 <= code && code <= 0x46 then
    16 * total + (10 + code - 0x41)
  else
    16 * total + (10 + code - 0x61)


typevar : Parser Expr
typevar =
  succeed (\identity -> Variable identity)
    |= typevarHelp

typevarHelp : Parser String
typevarHelp =
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList [ "if", "then", "else", "while" , "do", "end", "for"]
    }

----------------------------------------------------------
-- FUNC def  start 
----------------------------------------------------------
type ArgValue
    = AvInt    Int
    | AvBool   Bool
    | AvFloat  Float
    | AvString  String
    | AvVar     String

type alias Input
    = Array.Array ArgValue

-------------------------------------------------------------

stringValue : Parser ArgValue
stringValue =
  --succeed   (::)
  succeed   Just
    |. spaces
    |. symbol "\""
    |= getChompedString (chompWhile (\c -> c /= '"'))
    |. symbol "\""
    |. spaces
    |> andThen
            (\( arg ) ->
                 succeed (AvString (arg   |> Maybe.withDefault "" ))
            )


intValue : Parser ArgValue
intValue =
  --succeed (::)
  succeed   Just
    |. spaces
    |= int
    |. spaces
    |> andThen
            (\( arg ) ->
                 --succeed (AvInt arg)
                 succeed (AvInt (arg   |> Maybe.withDefault 0 ))
            )

floatValue : Parser ArgValue
floatValue =
  --succeed (::)
  succeed   Just
    |. spaces
    |= float
    |. spaces
    |> andThen
            (\( arg ) ->
                 --succeed (AvFloat arg)
                 succeed (AvFloat (arg   |> Maybe.withDefault 0.0 ))
            )


varValue : Parser ArgValue
varValue =
  succeed (\identity -> AvVar identity)
    |= varValueHelp

varValueHelp : Parser String
varValueHelp =
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList [ "if", "then", "else", "while" , "do", "end", "for"]
    }
---------------------------------------------
{--
varValue : Context -> Parser ArgValue
varValue context =
  succeed   Just
    |. spaces
    |= (var context)
    |. spaces
    |> andThen
            (\( arg ) ->
                 let
                   r = case arg of
                        Just (IntT n) ->
                                 (AvInt   n)
                        Just (FloatT n) ->
                                 (AvFloat n)
                        Just (StringT n)  ->
                                 (AvString n)
                        Just (BoolT n) ->
                                 (AvBool  n)
                        Nothing ->
                                 (AvInt -1)
                 in
                 succeed (r)
            )
--}
---------------------------------------------
argValues :  Parser (List ArgValue)
argValues  =
  succeed (::)
    |. spaces
    |= oneOf
        [ backtrackable  stringValue
        , backtrackable  intValue
        , backtrackable  floatValue
        , varValue
        ]
    |. spaces
    |= argValuesTail 
    |> andThen
            (\( arg ) ->
                 succeed (arg)
            )

argValuesTail :  Parser (List ArgValue)
argValuesTail  =
  oneOf
    [ succeed (::)
        |. symbol ","
        |. spaces
        |= oneOf
            [ backtrackable  stringValue
            , backtrackable  intValue
            , backtrackable  floatValue
            , varValue
            ]
        |. spaces
        |= lazy (\_ ->  argValuesTail )
    , succeed []
    ]

-------------------------------------------------------------

func :  Parser Expr
func  =
    succeed Tuple.pair
        |= backtrackable
            (variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.empty
                }
            )
        |. backtrackable (symbol "(")
        --|= stringValues
        --|= oneOf
        --    [ backtrackable  stringValues     -- ("AAA","BBB","CCC")
        --    , backtrackable  intValues        -- (1, 2, 3)
        --    ,  floatValues
        --    ]
        |= argValues 
        |. symbol ")"
        |> andThen
            (\( name, arg ) ->
                --let _ = Debug.log "func arg parse ..." 0 in
                let
                  --base = case arg of
                  --         ListString arg_  ->
                  --                let _ = Debug.log "ListString" 0 in
                  --                ArrayString (Array.fromList arg_)
                  --         ListInt arg_  ->
                  --                let _ = Debug.log "ListInt" 0 in
                  --                ArrayInt (Array.fromList arg_)
                  --         ListFloat arg_  ->
                  --                let _ = Debug.log "ListFloat" 0 in
                  --                ArrayFloat (Array.fromList arg_)

                  base = Array.fromList arg

                in
                succeed (Function name  base)
            )


----------------------------------------------------------
-- FUNC def   end
----------------------------------------------------------

-- PARSER


{-| We want to handle integers, hexadecimal numbers, and floats. Octal numbers
like `0o17` and binary numbers like `0b01101100` are not allowed.

    run digits "1234"      == Ok (Integer 1234)
    run digits "-123"      == Ok (Integer -123)
    run digits "0x1b"      == Ok (Integer 27)
    run digits "3.1415"    == Ok (Floating 3.1415)
    run digits "0.1234"    == Ok (Floating 0.1234)
    run digits ".1234"     == Ok (Floating 0.1234)
    run digits "1e-42"     == Ok (Floating 1e-42)
    run digits "6.022e23"  == Ok (Floating 6.022e23)
    run digits "6.022E23"  == Ok (Floating 6.022e23)
    run digits "6.022e+23" == Ok (Floating 6.022e23)
    run digits "6.022e"    == Err ..
    run digits "6.022n"    == Err ..
    run digits "6.022.31"  == Err ..

-}
digits : Parser Expr
digits =
  number
    { int = Just Integer
    , hex = Just Integer
    , octal = Nothing
    , binary = Nothing
    , float = Just Floating
    }

bool : Parser Expr
bool =
    let
        true =
            succeed (always (Bool True))
                |= keyword "True"
                |. spaces

        false =
            succeed (always (Bool False))
                |= keyword "False"
                |. spaces
    in
    oneOf [ true, false ]


term : Parser Expr
term =
  succeed (\a -> a)
    |. spaces
    |= oneOf
         --[ digits
         [ backtrackable string
         , backtrackable func
         , backtrackable typevar
         --, backtrackable func
         , backtrackable digits
         , bool
         , succeed identity
             |. symbol "("
             |. spaces
             |= lazy (\_ -> expression)
             |. spaces
             |. symbol ")"
         ]
    |. spaces

term2 : Parser Expr
term2 =
  oneOf
    [ digits
    , succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> expression)
        |. spaces
        |. symbol ")"
    ]


expression : Parser Expr
expression =
  term
    |> andThen (expressionHelp [])


{-| If you want to parse operators with different precedence (like `+` and `*`)
a good strategy is to go through and create a list of all the operators. From
there, you can write separate code to sort out the grouping.
-}
expressionHelp : List (Expr, Operator) -> Expr -> Parser Expr
expressionHelp revOps expr =
  oneOf
    [ succeed Tuple.pair
        |. spaces
        |= operator
        |. spaces
        |= term
        |> andThen (\(op, newExpr) -> expressionHelp ((expr,op) :: revOps) newExpr)
    , lazy (\_ -> succeed (finalize revOps expr))
    ]


type Operator = AddOp 
              | SubOp
              | MulOp
              | DivOp
              | Div2Op
              | Div3Op
              | AndOp
              | OrOp
              | LTOp
              | GTOp
              | LEOp
              | GEOp
              | EQOp
              | NEOp



operator : Parser Operator
operator =
  oneOf
    [ map (\_ -> AddOp) (symbol "+")
    , map (\_ -> SubOp) (symbol "-")
    , map (\_ -> MulOp) (symbol "*")
    --, map (\_ -> DivOp) ( backtrackable (symbol "//"))
    --, map (\_ -> DivOp) ( backtrackable (symbol "/"))
    , map (\_ -> Div2Op) (symbol "//")
    , map (\_ -> DivOp)  (symbol "/")
    , map (\_ -> Div3Op) (symbol "%")
    , map (\_ -> AndOp) (symbol "&&")
    , map (\_ -> OrOp)  (symbol "||")
    --, map (\_ -> LTOp)  (symbol "<")
    --, map (\_ -> GTOp)  (symbol ">")
    , map (\_ -> LEOp)  (symbol "<=")
    , map (\_ -> GEOp)  (symbol ">=")
    , map (\_ -> LTOp)  (symbol "<")
    , map (\_ -> GTOp)  (symbol ">")
    , map (\_ -> EQOp)  (symbol "==")
    , map (\_ -> NEOp)  (symbol "!=")
    ]


{-| We only have `+` and `*` in this parser. If we see a `MulOp` we can
immediately group those two expressions. If we see an `AddOp` we wait to group
until all the multiplies have been taken care of.

This code is kind of tricky, but it is a baseline for what you would need if
you wanted to add `/`, `-`, `==`, `&&`, etc. which bring in more complex
associativity and precedence rules.
-}
finalize : List (Expr, Operator) -> Expr -> Expr
finalize revOps finalExpr =
  case revOps of
    [] ->
      finalExpr

    (expr, MulOp) :: otherRevOps ->
      finalize otherRevOps (Mul expr finalExpr)
    (expr, DivOp) :: otherRevOps ->
      finalize otherRevOps (Div expr finalExpr)
    (expr, Div2Op) :: otherRevOps ->
      finalize otherRevOps (Div2 expr finalExpr)
    (expr, Div3Op) :: otherRevOps ->
      finalize otherRevOps (Div3 expr finalExpr)

    (expr, AddOp) :: otherRevOps ->
      Add (finalize otherRevOps expr) finalExpr
    (expr, SubOp) :: otherRevOps ->
      Sub (finalize otherRevOps expr) finalExpr

    (expr, AndOp) :: otherRevOps ->
      And (finalize otherRevOps expr) finalExpr
    (expr, OrOp) :: otherRevOps ->
      Or (finalize otherRevOps expr) finalExpr

    (expr, LTOp) :: otherRevOps ->
      LT (finalize otherRevOps expr) finalExpr
    (expr, GTOp) :: otherRevOps ->
      GT (finalize otherRevOps expr) finalExpr
    (expr, LEOp) :: otherRevOps ->
      LE (finalize otherRevOps expr) finalExpr
    (expr, GEOp) :: otherRevOps ->
      GE (finalize otherRevOps expr) finalExpr
    (expr, EQOp) :: otherRevOps ->
      EQ (finalize otherRevOps expr) finalExpr
    (expr, NEOp) :: otherRevOps ->
      NE (finalize otherRevOps expr) finalExpr
----------------

strjoin : Context -> Input -> OutVal
strjoin context ar  =
   let
       a_ = case (Array.get 0 ar) of
                  Just (AvString a)  ->
                           a 
                  _ ->
                           ""

       b_ = case (Array.get 1 ar) of
                  Just (AvString a)  ->
                           a 
                  Just (AvVar a)  ->
                           --"<<" ++ a  ++ ">>"
                           let
                              value = getConstant a context
                              ans_ = case value of
                                          Just v ->
                                                 v
                                          _ ->
                                                 (OString " AvVar not_found")
                              result = case ans_ of
                                          OString v ->
                                                 v
                                          _ ->
                                                 " AvVar not_found"
                           in
                           result

                  _ ->
                          ""

       ans = a_ ++  b_
   in
   OString ans

exec : String -> String
exec str =
 let
   ast = parse str
   result = case ast of
        Err err ->
           Debug.toString err
        Ok expr ->
           let
             context = empty
             context_ = addConstant "test1" (OString "OKOK") context
             context2 = addConstant "test_flort" (OFloat 10.1) context_
             context3 = addFunction "strjoin" strjoin  context2
             --ans = evaluate expr
             ans = evaluate context3 expr
           in
           Debug.toString ans
             
 in
 result

{--
> import TestExpr exposing (..)
> exec " 1 + 3 + (7 //2) "
"OFloat 7" : String
> exec " 1 + 3 + (7 /2) "
"OFloat 7.5" : String
> exec " 1 + 3 + (7 %2) "
"OFloat 5" : String
> exec " \"abc\" + \"ABC\" "
"OString \"abcABC\"" : String
> exec " False "
"OBool False" : String
> exec " True "
"OBool True" : String
> exec " True && True"
"OBool True" : String
> exec " True && False"
"OBool False" : String
> exec " True || True"
"OBool True" : String
> exec " True || False"
"OBool True" : String
> exec " \"abc\" + test1 "
"OString \"abcOKOK\"" : String
> exec " 1.1  + test_flort "
"OFloat 11.2" : String
> exec " \"abc\" + strjoin( \"ABC\", \"XYZ\") "
"OString \"abcABCXYZ\"" : Strin
> exec " \"abc\" + strjoin( \"ABC\", test1) "
"OString \"abcABCOKOK\"" : String

> exec "1.0 <= 100.1"
"OBool True" : String
> exec "1.0 < 100.1"
"OBool True" : String
> exec "1.0 > 100.1"
"OBool False" : String
> exec "1.0 >= 100.1"
"OBool False" : String
> exec "1.0 <= 100.1"
"OBool True" : String
> exec "1.0 == 100.1"
"OBool False" : String
> exec "1.0 != 100.1"
"OBool True" : String
> exec "1.1 == 1.1"
"OBool True" : String

> exec "e"
"OString \"not_found\"" : String

                                --array
> exec " [ 1,2,3,4,5] "
> exec " [ "1","2","3","4","5"] "
--}
