Module 08: Variables
====================

This module will be due at **1:15pm on Tuesday, October 3**.

* Write your team names here:

    Bosco Ndemeye
    Joseph Matson

    Source 1: https://wiki.haskell.org/All_About_Monads
    Source 2: https://www.haskell.org/tutorial/monads.html


In this module, we will explore how to support a language with
*variables*.

We'll start with the familiar Arith language.

> {-# LANGUAGE GADTs #-}
>
> import qualified Data.Map as M
> import Prelude
> import Parsing hiding ((<$>), (<$), (<*>))
> import Data.Maybe
>
> data Arith where
>   Lit :: Integer -> Arith
>   Bin :: Op -> Arith -> Arith -> Arith
>   Var :: String -> Arith
>   Let :: String -> Arith -> Arith -> Arith
>   deriving (Show)
>
> data Op where
>   Plus  :: Op
>   Minus :: Op
>   Times :: Op
>   Div   :: Op
>   deriving (Show, Eq)
>
> type Env = M.Map String Integer
>
> interpArith :: Arith -> Env -> Integer
> interpArith (Lit i)  e         = i
> interpArith (Bin Plus e1 e2) e  = interpArith e1 e + interpArith e2 e
> interpArith (Bin Minus e1 e2) e = interpArith e1 e - interpArith e2 e
> interpArith (Bin Times e1 e2) e = interpArith e1 e * interpArith e2 e
> interpArith (Bin Div e1 e2) e = interpArith e1 e `div` interpArith e2 e
> interpArith (Var s) e = fromJust (M.lookup s e)
> interpArith (Let s v a) e = interpArith a (M.insert s (interpArith v e) e)
>

>
> lexer :: TokenParser u
> lexer = makeTokenParser $ emptyDef
>   { reservedNames = ["let", "in"] }
>     -- tell the lexer that "let" and "in" are reserved keywords
>     -- which may not be used as variable names
>
> parens :: Parser a -> Parser a
> parens     = getParens lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer
>
> reserved :: String -> Parser ()
> reserved = getReserved lexer
>
> integer :: Parser Integer
> integer    = getInteger lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> identifier :: Parser String
> identifier = getIdentifier lexer
>
> parseArithAtom :: Parser Arith
> parseArithAtom = (Lit <$> integer)
>                  <|> parens parseArith
>                  <|> (Var <$> identifier)
>                  <|> (Let <$> ((reserved "let") *> identifier) <*> ((reserved "=") *> parseArith) <*> ((reserved "in") *> parseArith))
>
>
> parseArith :: Parser Arith
> parseArith = buildExpressionParser table parseArithAtom
>   where
>     table = [ [ Infix (Bin Times <$ reservedOp "*") AssocLeft ],
>               [Infix (Bin Div <$ reservedOp "/") AssocLeft]
>             , [ Infix (Bin Plus  <$ reservedOp "+") AssocLeft
>               , Infix (Bin Minus <$ reservedOp "-") AssocLeft]
>
>             ]
>
> arith :: Parser Arith
> arith = whiteSpace *> parseArith <* eof
>
> eval :: String -> IO ()
> eval s = case parse arith s of
>   Left m -> print m
>   Right e -> case interpArith3 e M.empty of
>                    Right n -> print n
>                    Left err       -> putStrLn (showInterpError err)

We will now add *variables* to this language.  In particular, we're
going to add *let-expressions*, which look something like this:

```
 >>> let x = 4*12 in x*x + 3-x
 2259
```
This locally defines `x` as a name for the value of `4*12` within the
expression `x*x + 3-x`.  Substituting `48` for each occurrence of `x`
and evaluating the result yields the final value of `2259`.

Haskell has let-expressions too, so you can try typing the above
expression at a GHCi prompt.

Syntax
------

* Choose a driver and write their name here:

Bosco

We need to make two changes to the syntax of the language.

- Arith expressions may now contain variables, represented as
  `String`s.
- Arith expressions may now contain let-expressions, which are of the
  form
    ```
    'let' <var> '=' <arith> 'in' <arith>
    ```

  where `<var>` is a variable name (a `String`) and the two
  occurrences of `<arith>` are Arith expressions.

* Add two new constructors to the definition of the `Arith` data type
  to represent these two new syntactic forms.  (Of course since
  `Arith` represents *abstract* syntax you do not need to worry about
  storing specific pieces of concrete syntax such as the word `let`
  and so on.)

  done.

* Modify the parser to parse the given concrete syntax.  A few hints:

    - You can use the provided `identifier :: Parser String` to parse
      a variable name.
    - You can use the provided `reserved :: String -> Parser ()` to
      parse the keywords `let` and `in`.  Notice how these keywords
      are specified as "reserved names" in the lexer, which means they
      may not be used as variable names.
    - You should extend the definition of `parseArithAtom` with two
      new cases, corresponding to the two new constructors of
      `Arith`. To keep the code a bit cleaner, you may want to create
      a new auxiliary definition called `parseLet` to parse a
      let-expression; or you can simply inline it into the definition
      of `parseArithAtom`.

* Be sure to test your parser on some sample inputs!  For example, you
  might try things like

    - `x+y-foo`
    - `let x = 4*48 in x*x + x-3`
    - `let x = 3 in let y = x*x in let z = y*y*x*x in z*z*y*y*x*x`
    - `(let baz = 19 in baz*baz) - (let foo = 12 in foo*foo)`

    but you should make up your own examples to try as well.
    All of these should parse successfully.

  Done!

![](../images/green.png)

Semantics
---------

* **ROTATE ROLES** and write the name of the new driver here:

Joseph

Now we will extend the interpreter appropriately.  The presence of
variables introduces two new wrinkles to our interpreter:

1. The interpreter will need to keep track of the current values of
   variables, so that when we encounter a variable we can substitute
   its value.
2. Variables introduce the possibility of runtime errors.  For
   example, `let x = 2 in x + y` will generate a runtime error, since
   `y` is never defined.

For now, you should deal with undefined variables simply by calling
the `error` function with an appropriate error message, which will
make the interpreter crash.  In a later section we will explore a
better way to handle runtime errors.

variable names to values, called an *environment*.  To represent such
a mapping, we can use the [`Data.Map`
module](http://hackage.haskell.org/package/containers-0.5.8.1/docs/Data-Map-Lazy.html).  Add the following line to the imports at the top of this file:

```
import qualified Data.Map as M
```

You can now refer to things from `Data.Map` by prefixing them with
`M.`, for example, `M.empty`, `M.insert`, `M.lookup`. (The reason for
prefixing with `M` like this is that otherwise there would be
conflicts with functions from the `Prelude`.)

* Define an environment as a map from variable names to values:
    ```
    type Env = M.Map String Integer
    ```

* Now add an extra `Env` parameter to `interpArith` which keeps track
  of the current values of variables.

* Extend `interpArith` to interpret variables and let-expressions.
  You will probably find the `M.insert` and `M.lookup` functions
  useful.  See the
  [`Data.Map` documentation](http://hackage.haskell.org/package/containers-0.5.8.1/docs/Data-Map-Lazy.html)
  for information on these (and other) functions.

* **Just for fun**: try to write down an expression which evaluates to
  an integer which is longer than the expression.  What is the shortest
  expression you can write with this property?
  "let x=9999 in x*x*x*x*x"

  Done!

![](../images/stop.gif)

Dealing with errors, take 1
---------------------------

* **ROTATE ROLES** and write the name of the new driver here:

Bosco

The interpreter works, but we have introduced the possibility of
runtime errors, and we are not dealing with them in a nice way: if a
variable is undefined, the interpreter simply crashes.  Ideally, the
interpreter should never crash, but instead return a useful error
value that can be caught and further processed however we wish.

To do this we will need to change the type of the interpreter again.
Currently, its type promises that it will always return an `Integer`,
but this is no longer possible: in case of a runtime error it will not
be able to return an `Integer`.  (Of course, we could arrange for it
to return some default `Integer` such as 0 in the case of a runtime
error, but there would then be no way to distinguish between an
expression that legitimately evaluated to 0 and one that resulted in a
runtime error.)

As we have seen (for example, in the type of `parseSome`), the
possibility of errors can be represented by the `Either` type.

* Create a new data type called `InterpError` to represent runtime
  errors generated by the interpreter.  For now, it should have only
  one constructor called `UndefinedVar`, containing a `String` (which
  represents the name of the undefined variable).

> data InterpError where
>     UndefinedVar :: String -> InterpError
>     DivisionByZero :: InterpError
>     deriving Show

* Our goal is to change the type of `interpArith` so that it returns
  `Either InterpError Integer` instead of just `Integer`.  However, at
  this point if we simply change its type it will not typecheck.

* Create a new function `interpArith2` which has the same type as
  `interpArith` except that it returns `Either InterpError Integer`
  instead of `Integer`.  For now, just make `interpArith2` handle
  `Var`, `Let`, `Bin Plus`, and `Lit`. (Don't bother implementing `Bin
  Minus` or `Bin Times`; they would be very similar to `Bin Plus`.)

> interpArith2 :: Arith -> Env -> Either InterpError Integer
> interpArith2 (Lit i)  e         = Right i
> interpArith2 (Bin Plus e1 e2) e  = case interpArith2 e1 e of
>                                     Right m -> case interpArith2 e2 e of
>                                                Right n -> Right (m + n)
>                                                k   -> k
>                                     k   -> k
> interpArith2 (Var s) e = case M.lookup s e of
>                                Just n -> Right n
>                                _ -> Left (UndefinedVar s)
> interpArith2 (Let s v a) e = case interpArith2 v e of
>                                Right m -> case interpArith2 a (M.insert s m e) of
>                                               Right n -> Right n
>                                               k      -> k
>                                k      -> k
>

    **WARNING**: this will be very annoying to write.  But unless you
    write this code you will not appreciate the nicer way we will
    implement it next!

* Write a function `showInterpError :: InterpError -> String` which
  displays a nice (human-readable) version of interpreter errors.

> showInterpError :: InterpError -> String
> showInterpError (UndefinedVar n) = n ++ " is not defined."
> showInterpError DivisionByZero = "error: Division by zero"

* Change the `eval` function to have type `String -> IO ()`, and
  change it to use `interpArith2` instead of `interpArith`.
    - If there is a parse error, display the parse error with the
      `print` function.
    - If there is a runtime error `err`, display it with `putStrLn
      (showInterpError err)`.
    - If the interpreter finishes successfully, display the result
      with `print`.

      Done!


* At this point you should be able to test `eval` with expressions
  that only contain `+`.  For example:
    - `2++`: should print a parse error.
    - `2+x`: should print an error message about `x` being undefined.
    - `2+3`: should successfully evaluate to `5`.

![](../images/green.png)

Dealing with errors, take 2
---------------------------

* **ROTATE ROLES** and write the name of the new driver here:

Joseph

The annoying thing about `interpArith2` is that it had to mix together
the actual work of interpreting with the work of doing case analysis
to figure out when a runtime error had occurred.  In this section we
will explore ways of hiding all the case analysis inside a few general
combinators which we can then use to implement the interpreter in a
nicer way.

* Start by creating a function `interpArith3` with the same type as
  `interpArith2`.  For now just implement the `Lit` and `Var` cases
  (you can probably just copy them from `interpArith2`).

> interpArith3 :: Arith -> Env -> Either InterpError Integer
> interpArith3 (Lit i)  e         = Right i
> interpArith3 (Var s) e = case M.lookup s e of
>                                 Just n -> Right n
>                                 _ -> Left (UndefinedVar s)
> --interpArith3 (Bin Div _ (Lit 0)) e  = Left DivisionByZero
> interpArith3 (Bin Plus e1 e2) e  = (+) <$> (interpArith3 e1 e) <*> (interpArith3 e2 e)
> interpArith3 (Bin Minus e1 e2) e = (-) <$> (interpArith3 e1 e) <*> (interpArith3 e2 e)
> interpArith3 (Bin Times e1 e2) e = (*) <$> (interpArith3 e1 e) <*> (interpArith3 e2 e)
> interpArith3 (Bin Div e1 e2) e   = case interpArith3 e2 e of
>                                          (Right 0) -> Left DivisionByZero
>                                          _         -> (div) <$> (interpArith3 e1 e) <*> (interpArith3 e2 e)
> interpArith3 (Let s v a) e       = (interpArith3 v e) >>= (\m -> interpArith3 a (M.insert s m e))

* Now implement an operator `(<<$>>) :: (a -> b) -> Either e a ->
  Either e b`. (If your implementation typechecks, it is probably
  correct.) What does this operator do?

> (<<$>>) :: (a ->b) -> Either e a -> Either e b
> (<<$>>) a (Right b) = Right (a b)
> (<<$>>) _ (Left b) = Left b
>

* Implement another operator `(<<*>>) :: Either e (a -> b) -> Either e
  a -> Either e b`.

> (<<*>>) :: Either e (a -> b) -> Either e a -> Either e b
> (<<*>>) (Right f) (Right a) = Right (f a)
> (<<*>>) (Left e) _          = Left e
> (<<*>>) (Right f) (Left e) = Left e

* Now try things like
    - `(+) <<$>> Right 2     <<*>> Right 5`
    - `(+) <<$>> Left "nope" <<*>> Right 6`
    - `(+) <<$>> Right 2     <<*>> Left "uhuh"`
    - `(+) <<$>> Left "nope" <<*>> Left "uhuh"`

    Explain what the pattern `f <<$>> ... <<*>> ... <<*>> ...` does.



* Where have you seen something like this before?

Parsing

* Now implement all the `Bin` cases for `interpArith3` using these new
  operators.


Done!


Now we only have the `Let` case remaining.  It turns out that
`(<<$>>)` and `(<<*>>)` are not enough to implement the `Let` case,
since the two recursive calls to `interpArith3` are not independent
(the second recursive call needs to use an environment extended with
the result of the first recursive call).

* Finish the implementation of `(>>>=)` below.  Note that the second
  argument to `(>>>=)` is a *function* which takes an `a` and returns
  an `Either e b`.  What does this operator do?

> (>>>=) :: Either e a -> (a -> Either e b) -> Either e b
> Left e1 >>>= _ = Left e1
> Right a >>>= f = f a

* Now use `(>>>=)` to implement the `Let` case of `interpArith3`.

Dealing with errors, take 3
---------------------------

As you have probably noticed, `(<<$>>)` and `(<<*>>)` are similar to
the `(<$>)` and `(<*>)` operators for parsers.  In fact, they are both
instances of a more general pattern.

* Move the phrase `hiding ((<$>), (<$), (<*>))` from after `Prelude`
  to after `Parsing`.  Previously, we were hiding the more general
  versions from the `Prelude` and using more specific ones defined in
  `Parsing`.  Now we will do the opposite.

* Now change all the uses of `(<<$>>)` and `(<<*>>)` in `interpArith3`
  to `(<$>)` and `(<*>)`.  Confirm that your code still typechecks and
  behaves the same way.

  Done!

* Look at the types of `(<$>)` and `(<*>)` in GHCi.  You can see that
  they have the same shape as the specific versions for `Parser` and
  `Either`, but work over any type `f` which supports the same sort of
  pattern.

* It turns out that `(>>>=)` corresponds to something more general
  too: change the use of `(>>>=)` to `(>>=)`, and confirm that your
  code still works.
Done!
* As a final comprehensive exercise, extend the language with a
  division operator. (You can use the Haskell `div` function to
  perform integer division.)  The interesting thing about division is
  that it introduces the possibility of a different kind of runtime
  error, namely, division by zero.  Extend the interpreter so that it
  does not crash but instead generates an appropriate error when
  division by zero is attempted.

Feedback
--------

* How long would you estimate that you spent working on this module?

* Were any parts particularly confusing or difficult?

* Were any parts particularly fun or interesting?

* Record here any other questions, comments, or suggestions for
  improvement.
