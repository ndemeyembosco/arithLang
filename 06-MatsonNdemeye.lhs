Module 06: The Arith language: parsing
======================================

* Write your team names here:

Bosco Ndemeye
Joseph Matson

* You may again choose whoever you want to start as the driver.  Write
  your choice here:

  Bosco

In this module, we will explore some rudiments of parsing.

> {-# LANGUAGE GADTs #-}
>
> import Data.Char   -- for isSpace, isDigit

Tokenizing
----------

> data Token where
>   TLit   :: Integer -> Token
>   TPlus  :: Token
>   TMinus :: Token
>   TTimes :: Token
>   LParen :: Token
>   RParen :: Token
>   deriving (Show, Eq)

* Write a function `tokenize :: String -> [Token]` which turns a
  string such as `" ( 2+ 45)"` into a list of tokens like
  `[LParen, TLit 2, TPlus, TLit 45, RParen]`.  Your function should
  ignore any whitespace (use the `isSpace` function).  *Hint*: you may
  also find the `span` function useful.  Try calling `span isDigit` to
  see what it does.

> tokenize :: String -> [Token]
> tokenize "" = []
> tokenize (x:xs)
>    |isDigit x = case span isDigit (x:xs) of
>                     (m, n) -> (TLit (read m)) : (tokenize n)
>    |isSpace x = tokenize xs
>    |otherwise = case x of
>       '(' -> LParen : (tokenize xs)
>       '+' -> TPlus  : (tokenize xs)
>       '-' -> TMinus : (tokenize xs)
>       '*' -> TTimes : (tokenize xs)
>       ')' -> RParen : (tokenize xs)
>       --otherwise -> undefined


    Of course, `tokenize` might fail if the provided `String` is not a
    valid Arith expression.  Ideally, we would handle the errors in a
    principled way (*e.g.* by having `tokenize` return a `Maybe
    [Token]`, or something even more informative). For now, your
    function should simply crash when given invalid input.  We will
    consider error handling in more depth later in the semester.

Postfix
-------

* **ROTATE ROLES** and write the name of the new driver here:
Joseph

Recall our representation of the Arith language from last time:

> data Op where
>   Plus  :: Op
>   Minus :: Op
>   Times :: Op
>   deriving (Show, Eq)
>
> data Arith where
>   Lit :: Integer -> Arith
>   Bin :: Op -> Arith -> Arith -> Arith
>   deriving (Show)

Consider the following alternative *postfix* syntax for Arith:

```
<digit> ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
<num>   ::= <digit>+
<op>    ::= '+' | '-' | '*'
<post>  ::= <num>
          | <post> ' ' <post> ' ' <op>
```

Notice that each operator appears *after* its operands (instead of
*between* them).

* Write down three examples that match the grammar `<post>`.

"1 2 +", "1 2 3 + -", "1 2 + 3 *"

* Consider the following infix expressions.  Write each as an
  equivalent postfix expression.

    + `3 + 4 * 5` = "3 4 + 5 *"
    + `3 * 4 + 5` = "3 4 * 5 +"
    + `(2 + 3 - (4 - 5)) * 6` = "2 3 + 4 5 - - 6 *"

Source : Corrected by Dr. Yorgey.

* Consider the abstract syntax trees for each of the above
  expressions.  What is the relationship between their trees and their
  postfix forms?

  it is the post order traversal of the tree

* Note that `<post>` expressions do not contain any parentheses.
  Will this ever cause ambiguity or incorrect parsing?  If so, give an
  example; if not, explain why not.

  no. it goes in the correct order through the operation tree. and there is one
  and only one post order traversal of a tree.

Class discussion: shunting
--------------------------

![](../images/stop.gif)

Shunting
--------

* **ROTATE ROLES** and write the name of the new driver here:
Bosco

* Write a function `shunt :: [Token] -> [Token]` which converts an
  infix expression into postfix, implementing railyard shunting
  algorithm discussed in class. (*Hint*: make another "helper"
  function with type `[Token] -> [Token] -> [Token]`, which keeps
  track of both the input list of tokens as well as the temporary
  operator stack.)

>
>
>
>

![](../images/green.png)

Parsing
-------

Coming soon...
