Module 12: The untyped lambda calculus
======================================

This module is due at **1:15pm on Thursday, October 20**.

* Write your team names here:
  Joseph Matson
  Bosco Ndemeye



  Source 1: https://en.wikipedia.org/wiki/First-class_function
  Source 2: https://wiki.haskell.org/Anonymous_function 

* As the initial driver, pick someone who has not seen the lambda
  calculus before.  If all of your team members are familiar with the
  lambda calculus, then you should pick the person who comes up with
  the best idea for determining the first driver.
  Joseph
The language
------------

The language we will consider in this module has the following syntax:

```
<expr> ::=
  | <var>
  | <int>
  | <expr> '+' <expr>
  | '^' <var> '->' <expr>
  | <expr> <expr>
  | '(' <expr> ')'
```

It has integers and addition, just to give us something basic to
compute with.  It also has variables, and *lambda expressions* of the
form `^x -> e`, where `x` is a variable name and `e` is an expression
(where `x` is bound in `e`).  This represents a function which takes
the parameter `x` as input and yields `e` as its
output.^[In Haskell, a backslash is used for lambda expressions (it is
supposed to look similar-ish to a lower case Greek lambda (λ), the
traditional notation).  However, in our context it would be annoying
to use a backslash since it also serves as an escape character in string
literals, so to write something like `\x -> \y -> x + y` as a Haskell
string you have to write `"\\x -> \\y -> x + y"`.]
Finally, our language has *function application* which is written like
`f x`, just as in Haskell.  Also as in Haskell, we consider function
application to associate to the left, so `f x y` is the same as `(f x)
y`.  If you take away integers and addition, this language is known as
the *lambda calculus*. (It turns out that you can encode integers and
addition, and much else besides, using only functions and function
application, but we won't get into that!)  It is a very simple model
of a language with (one-argument) functions.

* Write down an example string which conforms to the concrete syntax
  of this language and uses every case in the grammar at least once.
"(^x -> x + (^y -> 3 + y) 5) 4"

* Make an algebraic data type called `Expr` to represent the abstract
  syntax of this language.

> {-# LANGUAGE GADTs #-}
>
> import Parsing2
> import qualified Data.Map as M
>
> data Expr where
>     V :: String -> Expr
>     I :: Integer -> Expr
>     Sum :: Expr -> Expr -> Expr
>     Func :: String -> Expr -> Expr
>     App :: Expr -> Expr -> Expr
>     deriving Show


![](../images/stop.gif)

Parsing
-------

* Complete the parser below by replacing any occurrences of
  `undefined`.  Be sure to download [Parsing2.hs](Parsing2.hs) and
  place it in the same directory.

    *Hint*: application should be thought of as a left-associative binary
    operator with very high precedence.  The only strange thing is that
    name of the operator is the empty string!  That is, somewhere in your
    parser you should have something like

    ```
    Infix (... <$ reservedOp "") AssocLeft
    ```

    (Of course, the dots should be filled in by an appropriate
    constructor from your AST.)  This is a weird trick but it works
    great.


>
> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>
> parens :: Parser a -> Parser a
> parens = getParens lexer
>
> identifier :: Parser String
> identifier = getIdentifier lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> integer :: Parser Integer
> integer = getInteger lexer
>
> parseAtom :: Parser Expr
> parseAtom
>   =   V  <$> identifier
>   <|> I  <$> integer
>   <|> Func <$> (reservedOp "^" *> identifier) <*> (reservedOp "->" *> parseExpr)
>   <|> parens parseExpr
>
> parseExpr :: Parser Expr
> parseExpr = buildExpressionParser table parseAtom
>   where
>     table = [ [ Infix (App <$ reservedOp "") AssocLeft]
>             , [ Infix (Sum <$ reservedOp "+") AssocLeft]
>             ]
>
> expr :: Parser Expr
> expr = whiteSpace *> parseExpr <* eof

You can use this function for testing:

> p :: String -> Expr
> p s = case parse expr s of
>   Left err -> error (show err)
>   Right e  -> e

![](../images/stop.gif)

Semantics and substitution
--------------------------

* **ROTATE ROLES** and write the name of the new driver here:

Bosco

The central idea behind the semantics for the lambda-calculus is what
happens when a lambda is applied to an argument: in `(^x -> e1) e2`,
the function `^x -> e1` is expecting an argument `x`, and it is
applied to an argument `e2`, so `(^x -> e1) e2` should reduce to *`e2`
with `e1` substituted for `x`*.  For example,
```
(^x -> x + x + 3) 6 --> 6 + 6 + 3 --> 15.
```

This is clear enough in theory, and one can go on to define a
small-step interpreter for the lambda calculus along these lines, as
you did in the previous module.  However, it is *very tricky* to get
substitution right!  In fact, your implementation of `subst` in the
previous module is almost certainly wrong, although it did not cause a
problem because of the limited way we made use of it.

To see the problem, consider the following expression:
```
let x = 6 in (let y = x + 2 in (let x = 3 in x + y))
```

* What should this expression evaluate to?

11.

Now suppose we tried to evaluate this expression a different way, by
first reducing the subexpression `let y = x + 2 in (let x = 3 in x +
y)` before reducing the outer `let`.

* What happens if you reduce `let y = x + 2 in (let x = 3 in x + y)`
  directly, by substituting `x + 2` for `y`?

  8

* Why do you think this is called a *capturing substitution*?

 Because the outer is like a cage for the inner one, where they are inside it
 but they can still move around.

* Your solution to the previous module probably did not run into this
  problem.  Why not?  (*Hint*: at the point where you call `subst x e1
  e2`, what can you say about `e1`?)


  Because we already know that e1 is one of the basic types when we call subst.
  and because it substitutes from the outside in.

One solution to this problem is to consistently rename things in order
to avoid name conflicts.  For example:

```
let x = 6 in (let y = x + 2 in (let x = 3 in x + y))
let x = 6 in (let y = x + 2 in (let q = 3 in q + y))
let x = 6 in (let q = 3 in q + (x + 2))
let x = 6 in 3 + (x + 2)
3 + (6 + 2)
3 + 8
11
```

This works, but is inefficient; there are
[also](https://en.wikipedia.org/wiki/De_Bruijn_index)
[many](http://www.chargueraud.org/research/2009/ln/main.pdf)
[other](http://www.sciencedirect.com/science/article/pii/S0304397504004013)
[solutions](http://www.cs.cmu.edu/~fp/papers/pldi88.pdf) to this
problem [as well](http://adam.chlipala.net/papers/PhoasICFP08/)!

![](../images/stop.gif)

A big-step interpreter
----------------------

* **ROTATE ROLES** and write the name of the new driver here:
Joseph

In any case, we are going to sidestep the whole substitution morass by
taking a different approach.  Instead of a small-step interpreter
which relies on substitution, you will write a big-step interpreter.
This has its own difficulties: in particular, since a big-step
interpreter typically turns an expression into some sort of *value*,
we have to decide what kind of values our interpreter can generate.
Of course, integers should be one potential kind of value. But we also
have functions to contend with.

The approach we will take today is to represent function values in our
language as actual Haskell functions, of type `Value -> Either
InterpError Value`. That is, a function value is something which takes
a fully evaluated argument as input, and either produces a fully
evaluated `Value` as a result, or crashes with an `InterpError`.  Note
that this only works since Haskell has first-class functions!
(Another approach we will explore later, which works even in languages
without first-class functions, is to use *closures*.)


* First, define an algebraic data type to represent interpreter
  errors.  For now, just make a constructor to represent undefined
  variables; you can add other sorts of errors as they become
  necessary.

> data InterpError where
>     UndefinedVar :: InterpError
>     TypeError    :: InterpError
>     ApplicationErr :: InterpError
>     deriving Show

* Define an algebraic data type `Value` with two constructors:
  - one that just contains an `Integer`;
  - and another that contains a function of type `Value -> Either
    InterpError Value`.

> data Value where
>     In :: Integer -> Value
>     Fu :: (Value -> Either InterpError Value) -> Value

Note that you will not be able to derive `Show` for your `Value` type,
because it is impossible to inspect a function in any way other than
by applying it to an argument.  This is actually a problem, since it
makes it difficult to generate good error messages or to display the
output of the interpreter (using *closures* will solve this problem as
well).

* For now, write a function `showValue :: Value -> String` which
  displays integer values normally but displays function values as
  `"<function"`.

> showValue :: Value -> String
> showValue (In n) = show n
> showValue (Fu f) = "<function>"

* Finally, define a type `Env` as a `Map` from variable names
  (`String`s) to `Value`s.

> type Env = M.Map String Value

* Now write an interpreter, `interp :: Env -> Expr -> Either
  InterpError Value`. Some hints/notes:

    + The case for interpreting a lambda expression is a bit tricky,
      but you should be able to figure it out by following the types.

    + When you encounter an application, first interpret the left-hand
      side and make sure it is a function value.  If so, interpret the
      right-hand argument, and then pass the argument to the function.

    + The expression `"((^x -> (^f -> f x + f (x + 3))) 1) ((^x -> ^y
      -> x + y) 3)"` is a good test for your implementation.  It
      should evaluate to `11`.

> interp :: Env -> Expr -> Either InterpError Value
> interp e (V s) = case M.lookup s e of
>       Just n -> Right n
>       Nothing -> Left UndefinedVar
> interp e (I n) = Right (In n)
> interp e (Sum e1 e2) = case interp e e1 of
>       Right (In n) -> case interp e e2 of
>               Right (In m) -> Right (In(n + m))
>               _ -> Left TypeError
>       _ -> Left TypeError
> interp e (App e1 e2) = case interp e e1 of
>               Right (Fu f) -> case interp e e2 of
>                     Right n -> f n
>                     n -> n
>               _ -> Left ApplicationErr
> interp e (Func s m) = Right (Fu (\x -> interp (M.insert s x e) m))

> ev :: String -> String
> ev s = case parse expr s of
>     Right n -> case interp M.empty n of
>           Right m -> showValue m
>           Left m -> show m
>     Left n -> show n

  data Expr where
  >     V :: String -> Expr
  >     I :: Integer -> Expr
  >     Sum :: Expr -> Expr -> Expr
  >     Func :: String -> Expr -> Expr
  >     App :: Expr -> Expr -> Expr
  >     deriving Show


Feedback
--------

* How long would you estimate that you spent working on this module?
3

* Were any parts particularly confusing or difficult?
Interp was pretty hard, particularly Func and App
* Were any parts particularly fun or interesting?
The first half was fun
* Record here any other questions, comments, or suggestions for
  improvement.
