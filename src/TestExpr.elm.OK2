module TestExpr exposing (..)

import Parser exposing (..)


-- EXPRESSIONS


type Expr
  = Integer Int
  | Floating Float
  | String String
  | Bool Bool
  | Add Expr Expr   --[+]  String Float
  | Sub Expr Expr   --[-]  Float
  | Mul Expr Expr   --[*]  Float
  | Div Expr Expr   --[/]  Float
  | Div2 Expr Expr  --[//] Float
  | Div3 Expr Expr  --[%]  Float
  | And  Expr Expr  --[&&] Bool
  | Or   Expr Expr  --[||] Bool

type OutVal
  = OFloat  Float
  | OString String
  | OBool Bool


--evaluate : Expr -> Float
evaluate : Expr -> OutVal
evaluate expr =
  case expr of
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
       a_ = evaluate a
       b_ = evaluate b
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
       a_ = case (evaluate a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate b) of
               OFloat n -> n
               _ -> 0
     in
     OFloat ( a_ -  b_)

    Mul a b ->
     let
       a_ = case (evaluate a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate b) of
               OFloat n -> n
               _ -> 0
     in
     OFloat ( a_ *  b_)

    Div a b ->
     let
       a_ = case (evaluate a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate b) of
               OFloat n -> n
               _ -> 0
     in
     OFloat ( a_ / b_)

    Div2 a b ->
     let
       a_ = case (evaluate a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate b) of
               OFloat n -> n
               _ -> 0
     in
     OFloat ((toFloat ((floor a_) // (floor  b_))))

    Div3 a b ->
     let
       a_ = case (evaluate a) of
               OFloat n -> n
               _ -> 0
       b_ = case (evaluate b) of
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
       a_ = case (evaluate a) of
               OBool n -> n
               _ -> False
       b_ = case (evaluate b) of
               OBool n -> n
               _ -> False
     in
     OBool ( a_ && b_)

    Or a b ->
     let
       a_ = case (evaluate a) of
               OBool n -> n
               _ -> False
       b_ = case (evaluate b) of
               OBool n -> n
               _ -> False
     in
     OBool ( a_ || b_)


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
         [ digits
         , string
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

----------------

exec : String -> String
exec str =
 let
   ast = parse str
   result = case ast of
        Err err ->
           Debug.toString err
        Ok expr ->
           let
             ans = evaluate expr
           in
           Debug.toString ans
             
 in
 result


