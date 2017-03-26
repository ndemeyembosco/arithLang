Module 07: Parser combinators
=============================

* Write your team names here:

Bosco Ndemeye
Joseph Matson

In practice, one rarely writes parsers from scratch like we did last
week.  Typically, one uses some sort of tool or framework for
constructing parsers, which hides a lot of the complexity of dealing
with lexing/tokenizing, precendence and associativity, *etc.*, and
allows you to focus more directly on the grammar you wish to parse.

In this module, we will explore a Haskell library for constructing
parsers called `parsec`.

This module is **for the entire week of September 13 and 15**.  It
is due at **1:15pm on Tuesday, September 20**.

> {-# LANGUAGE GADTs #-}
>
> -- Hide the standard <$>, <$, and <*> operators so we can use
> -- variants with more specific types
> import Prelude hiding ((<$>), (<$), (<*>))
>
> -- Parsing is a module I have provided for you which wraps up some
> -- functionality of parsec into a somewhat easier/simpler interface.
> import Parsing
> import Text.Parsec.Combinator (choice)
> import Text.Parsec.Char (digit)
> import Data.Char (isDigit, digitToInt)
> -- Our old friend Arith
> data Arith where
>   Lit :: Integer -> Arith
>   Add :: Arith -> Arith -> Arith
>   Sub :: Arith -> Arith -> Arith
>   Mul :: Arith -> Arith -> Arith
>   Exp :: Arith -> Arith -> Arith
>   Neg :: Arith -> Arith
>   deriving (Show)
>
> interpArith :: Arith -> Integer
> interpArith (Lit i) = i
> interpArith (Add e1 e2) = interpArith e1 + interpArith e2
> interpArith (Sub e1 e2) = interpArith e1 - interpArith e2
> interpArith (Mul e1 e2) = interpArith e1 * interpArith e2
> interpArith (Exp e1 e2) = interpArith e1 ^ interpArith e2
> interpArith (Neg i) = - (interpArith i)
>
> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>
> parens :: Parser a -> Parser a
> parens     = getParens lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer
>
> integer :: Parser Integer
> integer    = getInteger lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> parseArithAtom :: Parser Arith
> parseArithAtom = (Lit <$> integer) <|> parens parseArith
>
> parseArith :: Parser Arith
> parseArith = buildExpressionParser table parseArithAtom
>   where
>     table = [[ Infix (Exp <$ reservedOp "^") AssocRight]
>             ,[ Infix (Mul <$ reservedOp "*") AssocLeft ]
>             ,[ Prefix (Neg <$ reservedOp "-")
>             , Infix (Add <$ reservedOp "+") AssocLeft
>               , Infix (Sub <$ reservedOp "-") AssocLeft
>               ]
>             ]
>
> arith :: Parser Arith
> arith = whiteSpace *> parseArith <* eof
>
> eval :: String -> Maybe Integer
> eval s = case parse arith s of
>   Left _  -> Nothing
>   Right e -> Just (interpArith e)

Token parsers
-------------

* Choose who will start out as the driver, and write their name here:
Bosco

The first thing to consider is `lexer`, which is a `TokenParser`.  For
now, we are using a simple default `TokenParser`; later we will see
how to customize it.  Essentially, `lexer` is an automatically
generated collection of special parsers which do the low-level work of
tokenizing.  The functions `getInteger`, `getParens`, *etc.* extract
these individual parsers from `lexer`.  We have extracted four such
token parsers and given them names to make them easier to use: one to
parse integers, one for whitespace, one for operators, and one to
parse parenthesized things.  (See the
[definition of `GenTokenParser`](http://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Token.html)
for a full list of the available token parsers.)

Parsers have a type like `Parser a`; for example, `integer` has type
`Parser Integer`. This means it is a parser which consumes some part
of a `String` and either returns a value of type `Integer` or fails
with parse error.  You can think of it like this:

`Parser a == String -> Maybe (a, String)`

although the actual definition is quite a bit more complicated.

You can use the `parseSome` function (provided in the `Parsing`
module) to try a parser and see what part of the input it consumes and
what part is left to be consumed by subsequent parsers.

* Try the `integer` parser using `parseSome`.  For example, you could
  evaluate `parseSome integer "23"` in GHCi.  Explain how `integer`
  behaves.  Be sure to try lots of different inputs: try spaces before
  and/or after an integer, try a negative sign, try non-digit
  characters before and after, *etc.* (You do not have to record the
  results of all your experiments.)  Explain in detail what the
  `integer` parser does.

  The integer parser parses the string looking for integers at the beginning and
  returns Right (integers, rest) where integers are the integers at the beginning and
  rest is the rest of the string.

  if there are no integers in front, it returns Left(line 1, column 1) and says that
  the first letter of the string was unexpected.



* Try each of the following and explain what they do.  Just as with
  `integer`, be sure to try many different inputs.

    * `reservedOp`

 calling (parseSome reservedOp str) on a string returns Right ((), rest) if
 the first character in the string matches the string `str`. Otherwise, it returns
 Left(Line 1, column n+1), where n is the index of the character on which the error was found.

    * `whiteSpace`

whiteSpace strips all the whitespace at the beginning of the string. It will never give an error, since
an empty string is also considered as a whitespace.

    * `parens`

parens returns Right (a, b) where a is the result of the given parser within the parenthesis at the beginning
of the string. it throws and error if there is anything you're not looking for between the parenthesis or
if there no parenthesis etc.

* In general, how do token parsers like `integer`, `reservedOp`, and
  `parens` handle spaces?

  In general they throw an error if there is a space before, and they delete the whitespace immediately
  following.

  integer is weird because it also deletes the spaces preceding unless it's a negative integer.

![](../images/stop.gif)

Parser combinators
------------------

* **ROTATE ROLES** and write the name of the new driver here:

Joseph

The token parsers provide the primitive building blocks out of which
we can construct more complicated parsers.  Now we will explore some
of the functions for building up more complex parsers.  Such functions
that allow building more complex things out of simpler parts are known
as *combinators*.

* Try `integer <|> parens integer`. (As before, "try" means "use the
  `parseSome` function to try it on various example inputs.")

    * What does `integer <|> parens integer` do?
    It parses the string looking for something that matches with integer or with
    parens integer

    * What is the type of `(<|>)`?

    Parser a -> Parser a -> Parser a

    * What does `(<|>)` do in general?

    It takes two parsers and makes one single parser.

* Now consider `Lit <$> integer`.

    * What does `Lit <$> integer` do?/
    This parses for integers, but returns them as Lit n.


    * What is the type of `(<$>)`?
     (a -> b) -> Parser a -> Parser b

    * What is the type of `Lit`?
    Integer -> Arith

    * What is the type of `Lit <$> integer`?  What does it do?
    Parser Arith


* Try `integer *> integer`.

    * What does `integer *> integer` do?

    It looks for an integer, then a space, and another integer. It essentially ignores
    the first integer and space, and parses integer on the remainder, starting at the second integer.

    * What is the type of `(*>)`?
    Applicative f => f a -> f b -> f b

    * What does `(*>)` do?
    It runs the first function, then runs the second, ignoring the first results, and
    outputting the results of the second.

* What do you think `(<*)` does?  Guess before trying it.
  This probably does the opposite, it runs the first and then the second, returning the
  results of the first.

* Now try it.  Were you right?
  essentially, it returned the primary ouput of the first function, and the remainder from the
  second.

* Try `"I" <$ integer`.

    * What does `"I" <$ integer` do?
      This parses integer but instead of returning the integer,
      it returns "I" and the remainder of the string.


    * What is the type of `(<$)`?
      a -> Parser b -> Parser a

    * What does `(<$)` do?

      It gives some a, and a Parser for b, and parses for b but returns a as the result,
      with the remainder of the string.

    * How is `(<$)` similar to `(<*)`?  How is it different?

    they are similar because both throw away part of the result, but different because
    (<$) replaces the first part, and (<*) throws away the middle

![](../images/green.png)

Combinator exercises
--------------------

* **ROTATE ROLES** and write the name of the new driver here:

Bosco

* Write a parser which parses two integers, discards the first one,
  and returns the second wrapped in a `Lit` constructor.  Be sure to
  test it!

> parser2Int :: Parser Arith
> parser2Int  = integer *> (Lit <$> integer)

* Write a parser which parses an integer surrounded by `~` on either
  side (for example, `"~23~"`) and returns the integer.

> parser3 :: Parser Integer
> parser3 = (reservedOp "~") *> integer <* (reservedOp "~")

* Skim through the `parsec` documentation for the following modules:
    * [`Text.Parsec.Char`](http://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Char.html)
    * [`Text.Parsec.Expr`](http://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Expr.html)
    * [`Text.Parsec.Combinator`](http://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Combinator.html)

    Note, wherever you see something like `ParsecT s u m a` you should
    think of it as `Parser a` (`ParsecT s u m a` is a more general
    version of `Parser a`).

    Pick one of the combinators you find and demonstrate its use.  You
    will have to add an `import` statement to the top of this file of
    the form:
    ```
    import Text.Parsec.SomeModule (combinatorYouChose)
    ```

> test :: Bool
> test = parseSome (choice [reservedOp "%", reservedOp "$", reservedOp "&"]) "&and$abc" == Right ((), "and$abc")

![](../images/stop.gif)

Building an Arith parser
------------------------

* **ROTATE ROLES** and write the name of the new driver here:

Joseph

Now we will explore how the token parsers and combinators are used to
build a parser for the Arith language.

Note that we have defined two parsers of type `Parser Arith`, namely,
`parseArith` and `parseArithAtom` (not counting `arith`, which we will
discuss later).  This is a common pattern when building these sorts of
parsers. The idea is that an "atomic" thing is something which forms
an indivisible unit, which we know how to parse just by looking at the
first token.  A non-atomic thing might be more complicated, for
example, it might involve infix operators.

* Explain what `parseArithAtom` does.

parseArithAtom parses and if there is an integer it returns it as a Lit, otherwise it
parses inside the parentheses for other Arith groupings using the other parseArith.

Now look at the definition of `parseArith`.  It uses a function
provided by `parsec` called `buildExpressionParser`, which deals
with parsing infix operators with various precedence levels and
associativities (using similar algorithms to those we explored in
the previous module).

* Explain what the parser `(Mul <$ reservedOp "*")` does.  What is its
  type?
  It parses for the "*" operator, and if it finds it it returns as the
  Arith Mul.
  The type is Parser (Arith -> Arith -> Arith), so it is a parser for Mul.

Notice that `table` consists of a list with two elements, each of
which is itself a list.  The first list has one element which refers
to `Mul`; the second list has two elements referencing `Add` and
`Sub`.

* What do you think this list signifies?
Since Mul is split from Add and Sub, it seems like this signifies the precedence of
the operators being parsed.

* Test your theory by switching the order of the two lists, and doing
  some test parses. Were you right? (Then switch them back.)

  Yes it seems we were right, switching the order of the lists reversed the assumed
  precedence of operators.

  We read more about it here:
  https://hackage.haskell.org/package.parsec-3.1.11/docs/Text-Parsec-Expr.html


* Extend Arith with an exponentiation operator.  Remember that
  exponentiation has higher precedence than multiplication and is
  right-associative. (The exponentiation operator in Haskell is
  `(^)`.) You will have to modify the `Arith` data type, the
  interpreter, and the parser.  Be sure to test that it works
  correctly.

 DONE

* Now extend Arith with prefix negation.  For example, `"-(2+3)"`
  should evaluate to `-5`.  Again, you should modify the `Arith`
  definition, parser, and interpreter.  When modifying the parser, you
  can use `Prefix` instead of `Infix`.  See the documentation for [`Text.Parsec.Expr`](http://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Expr.html).

DONE


The `(<*>)` operator
--------------------

There is one more parser operator we need to learn about, namely,
`(<*>)`.  It wasn't needed in the Arith parser but will often be useful.

* What is the type of `(<*>)`?
 it's type signature is : (<*>) :: Parser (a -> b) -> Parser a -> Parser b

Unfortunately, the type of `(<*>)` does not give you a good sense of
how to use it unless you have already spent a good deal of time
thinking about higher-order functions, currying, and the like in
functional programming.  Instead, we'll look at some examples.

> add :: Integer -> Integer -> Integer
> add x y = x + y

* Test the parser `(add <$> integer <*> integer)`.  What does it do?

 It parsers for integer twice and then adds the results of the parsing.

(In fact, we could also have written `((+) <$> integer <*> integer)`,
which would do exactly the same thing.)

* Write a function `add3` which takes three `Integer`s and adds them.

> add3 :: Integer -> Integer -> Integer -> Integer
> add3 n m t = (n + m) + t

* Now test the parser `(add3 <$> integer <*> integer <*> integer)`.
  What does it do?

 parses for integer three times, and adds the results of the parsing.

* In general, what do you think `(func <$> parser1 <*> parser2 <*> ... <*> parserN)` does?

It runs the parsers and then feeds the results of the parsers as input to the function.

* Write a parser `adder :: Parser Arith` which expects to read two integer values, and returns an `Arith` value consisting of an `Add` node containing the two integers.  For example:

    ```
    parseSome adder "32 9 xyz"
    Right (Add (Lit 32) (Lit 9), "xyz")
    ```

> makeAdd :: Integer -> Integer -> Arith
> makeAdd n m = Add (Lit n) (Lit m)

> adder :: Parser Arith
> adder =  makeAdd <$> integer <*> integer

  BIN and EBIN parsers
  --------------------

  * **ROTATE ROLES** and write the name of the new driver here:

  Bosco

  * Recall the BIN language from [module 4](04-syntax-semantics.html).
    As a reminder, the concrete syntax of BIN looks like this:

          <bin> ::= '#'
                  | '(' <bin> <bin> ')'

      Write a parser for BIN using parsec.  A few hints:

      + Define `symbol = getSymbol lexer` and use it to parse the `#`
        character.  Using `reservedOp` will not work since it wants to treat
        `##` as a single operator.


> symbol :: Parser String
> symbol = getSymbol lexer "#"
>
> data Bin where
>   Car :: Bin
>   Par  :: Bin -> Bin -> Bin
>   deriving Show

> parseBin :: Parser  Bin
> parseBin = (Car <$ (symbol)) <|> parens (Par <$> parseBin <*> parseBin)


  * Now extend your BIN parser to an EBIN parser.  Some hints:

      + Use the parser `digit :: Parser Char` to parse a single digit.

      + You will probably find it useful to write a function `mkLit ::
        Char -> EBin` which creates an `EBin` out of a `Char` which is a
        single digit.

> data EBin where
>   Mid :: EBin
>   Digit :: Int -> EBin
>   Lay :: EBin -> EBin -> EBin
>   deriving Show

> mkLit :: Char -> EBin
> mkLit n = if (isDigit n) then Digit (digitToInt n) else undefined

source : hoogle

> parseEBin :: Parser EBin
> parseEBin = (Mid <$ (symbol)) <|> (mkLit <$> (digit)) <|> parens (Lay <$> parseEBin <*> parseEBin)

  Feedback
  --------

  * How long would you estimate that you spent working on this module?

  * Were any parts particularly confusing or difficult?

  * Were any parts particularly fun or interesting?

  * Record here any other questions, comments, or suggestions for
    improvement.
