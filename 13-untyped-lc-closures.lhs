Module 13: Closures and types
=============================

This module is due at **1:15pm on Tuesday, October 25**.

* Write your team names here:

Bosco Ndemeye
Joseph Matson


Source 1 : https://wiki.haskell.org/Closure
Source 2 : https://mail.haskell.org/pipermail/beginners/2009-July/002067.html (We found you!)

In the previous module, you wrote an interpCreter for the untyped
lambda calculus (extended with integers and addition) by interpCreting
lambda-calculus integers and functions directly to *Haskell*
`Integer`s and functions.  This is in some sense the easiest
interpCreter to write.  However, it had a few problems:

1. The same implementation technique would not work in another
   language without first-class functions (say, C).  Even in languages
   like Java or Python that nominally do have first-class functions,
   it might be nicer to avoid them in this case.
2. Once we have a function, we cannot use or inspect it in any way
   other than applying it to arguments.  This made generating error
   messages or the results of the interpCreter more difficult, because
   there is no way to print out a function.

Today we will start by considering another way to implement the
interpCreter which solves these problems.

* Begin by copying in the code from your previous module.

> {-# LANGUAGE GADTs #-}
>
> import Parsing2
> import qualified Data.Map as M
>
> data Expr where
>     V :: String -> Expr
>     I :: Integer -> Expr
>     Sum :: Expr -> Expr -> Expr
>     Func :: String -> Maybe Type -> Expr -> Expr
>     App :: Expr -> Expr -> Expr
>     deriving Show

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
>   <|> Func <$> (reservedOp "^" *> identifier) <*> (reservedOp "[" *> optionMaybe parseType <* reservedOp "]") <*> (reservedOp "->" *> parseExpr)
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

> p :: String -> Expr
> p s = case parse expr s of
>   Left err -> error (show err)
>   Right e  -> e

> data InterpError where
>     UndefinedVar :: InterpError
>     TypeError    :: InterpError
>     ApplicationErr :: InterpError
>     deriving Show

> data Value where
>     In :: Integer -> Value
>     Fu :: String -> Expr -> Env -> Value



> showValue :: Value -> String
> showValue (In n) = show n
> showValue (Fu s e _) = show s ++ "->" ++ show e


> type Env = M.Map String Value

> interpC :: Env -> Expr -> Either InterpError Value
> interpC e (V s) = case M.lookup s e of
>       Just n -> Right n
>       Nothing -> Left UndefinedVar
> interpC e (I n) = Right (In n)
> interpC e (Sum e1 e2) = case interpC e e1 of
>       Right (In n) -> case interpC e e2 of
>               Right (In m) -> Right (In(n + m))
>               _ -> Left TypeError
>       _ -> Left TypeError
> interpC e (App e1 e2) = case interpC e e1 of
>               Right (Fu s m en) -> case interpC e e2 of
>                     Right n -> interpC (M.insert s n en) m
>                     n -> n
>               _ -> Left ApplicationErr
> interpC e (Func s _ m) = Right (Fu s m e)

> eval :: String -> IO ()
> eval s = case parse expr s of
>     Right n -> case interpC M.empty n of
>           Right m -> print (showValue m)
>           Left m -> print m
>     Left n -> print n


Closures
--------

The basic idea is to think of the value of a function as a sort of
*suspended computation*.  When the interpCreter hits a function, it
puts the function into "suspended animation"; when it is finally
provided with an argument then computation can resume. So when we
encounter a function we don't try to interpCret it any further, but just
save its parameter name and body in a *closure*, to be interpCreted
later.

However, this is not quite enough.  The body of a function might
contain variables, which need to be interpCreted in an environment.  When
we "suspend" a function, we need to be sure to *save the current
environment* along with it, so that when we wake it up to continue
executing, any variables it contains will be interpCreted in the
correct environment.  We call this combination of a function body and
its environment a *closure*.

* You should modify your `Value` type from before:
    - `Integer` values are exactly as before.
    - The other constructor is now different. Instead of storing an
      actual Haskell function, it should store *three* things:
        - a variable name (the function argument)
        - an expression (the function body), and
        - an environment.



* You should now be able to make a more satisfying `showValue`
  function which returns something better than `<function>` in the
  case of a closure.

* Now write a new interpCreter, `interpC :: Env -> Expr -> Either
  interpCError Value` (you may be able to reuse some parts of your
  interpCreter from the last module, but **be sure to replace recursive
  calls with `interpC`**!). Some hints/notes:

    + When you encounter a lambda, you should not evaluate it at all,
      but immediately turn it into a closure, also stashing away the
      *current* environment.

    + When you encounter an application, first interpCret the left-hand
      side and make sure it is a closure.  If so, interpCret the
      right-hand argument, and then interpCret the body of the closure
      under an appropriately extended environment.

    + You can use the expression `"((^x -> (^f -> f x + f (x + 3))) 1)
      ((^x -> ^y -> x + y) 3)"` to test whether your closures are
      working properly.  It should evaluate to `11`.

* To test your implementation, write a function `eval :: String -> IO
  ()` as usual, which takes a concrete expression and tries to parse
  and evaluate it.

![](../images/stop.gif)

Types for the lambda calculus
-----------------------------

* **ROTATE ROLES** and write the name of the new driver here:

  Joseph

As you have probably noticed, up until this point it is quite possible
to write nonsensical expressions like `3 6`, where `3` is applied to
the argument `6` as if it were a function.  As usual, we'd like to
create a type system and type checker which can rule out such
nonsensical expressions before running our programs.  This turns out
to be a bit more subtle than for Arith, and we will spend the next few
modules exploring the idea.  This module will just get you to start
thinking about some of the issues involved.

* Consider the following expressions.  Which do you think should be
  allowed, and which should not?  For those you think should be allowed,
  say what they evaluate to; for those that should not, explain why.

    1. `3 6` This shouldn't be allowed, 3 isn't a function.
    1. `(^x -> x)` This should be allowed, it's the "identity function" and evaluates to the input.
    1. `(^x -> 3) 5` Yes, this should be allowed, it always returns 3.
    1. `(^x -> 3) (^y -> y)` This should be allowed, because the input doesn't matter, evaluates to 3.
    1. `(^f -> f 5 + f 6) 2` This shouldn't be allowed, because the input needs to be a function.
    1. `(^f -> f 5 + f 6) (^z -> z + 1)` This should be allowed- it evaluates to 13
    1. `(^f -> (^x -> f (f x))) (^x -> x + x) 5` This should be allowed, because they are left associative.

* Our first thought might be to create a type system with just two
  types, `Integer` and `Function`.  Explain why this is not good
  enough. (*Hint*: remember that the goal of the type system is to be
  able to distinguish things that will crash at runtime from things
  that won't.  To help answer this question you might want to consider
  examples 5 and 6 above.  What should be the type of `(^f -> f 5 + f
  6)`?)

It's not a good idea, because sometimes functions take functions as inputs and sometimes
they take integers as functions.

In general, we need to know not just that something is a function, but
also what type it expects as an input, and what type it will return as
an output.  As in Haskell, we will write `in -> out` for a function
that expects an input of type `in` and yields a result of type `out`.

* Make a data type called `Type` to represent types for our language.
  There should be two constructors, representing `Integer` and
  function types respectively.

> data Type where
>     It :: Type
>     Ft :: Type -> Type -> Type
>     deriving Show

We can now think about how to figure out the type of a lambda
expression.

+ For example, consider `(^x -> x) 3`.  Since the lambda expression is
  applied to an argument of type `Integer`, the input `x` to the
  lambda expression must be an `Integer`; since `x` is also the
  output, the whole lambda expression `(^x -> x)` therefore must have type
  `Integer -> Integer`.

+ As another example, consider `(^f -> f 1) (^x -> 1)`.  First, `(^x
  -> 1)` obviously returns an `Int`.  But there is nothing to a priori
  tell us the type of `x`.  However, notice that `(^x -> 1)` is passed
  as the argument to `(^f -> f 1)`, which takes its argument `f` and
  applies it to the value `1` (which is obviously of type `Int`).
  Therefore `x` must have type `Int`, and `(^x -> 1)` has type `Int ->
  Int`.  Now, `(^f -> f 1)` takes an argument of type `(Int -> Int)`
  and applies it to `1`, which produces an `Int` as the output of the
  whole function.  So the type of `(^f -> f 1)` is `(Int -> Int) -> Int`.

* Your turn: give the type of each lambda expression in the allowed
  expressions from the previous exercise (the one beginning "Consider
  the following expressions...").

  1. `(^x -> x)` x = Integer -> Integer
  1. `(^x -> 3) 5` x = Integer -> Integer
  1. `(^x -> 3) (^y -> y)` x = (Integer -> Integer) -> Integer, y = Integer -> Integer
  1. `(^f -> f 5 + f 6) (^z -> z + 1)`f =  (Integer -> Integer) -> Integer, z = Integer -> Integer
  1. `(^f -> (^x -> f (f x))) (^x -> x + x) 5` f = (Integer -> Integer) -> (Integer -> Integer),
  x1 = (Integer -> Integer) -> Integer, x2 = Integer -> Integer


![](../images/green.png)

Type annotations
----------------

* **ROTATE ROLES** and write the name of the new driver here:
Bosco

As you probably found, inferring the types of arbitrary lambda
expressions requires some nontrivial logical deduction.  For now at
least, we're not even going to implement it in its most general form.
The main problem arises when we are asked to infer the type of
something like `(^x -> 3)`.  It is a function which returns an `Int`,
but what is the type of the input?  We cannot know without looking to
see how the function is used.  This makes type inference non-local,
and requires doing some kind of constraint solving.  This can be done
(for example, GHC does it), but it will make things much easier for
now if we just refuse to infer the type of a lambda expression like
this.  Instead, we can extend the syntax of lambda expressions to
allow specifying the type of the argument, like this: `(^ x [Int] ->
3)`.  This specifies that the argument `x` has type `Int`, so we can
infer that the whole lambda expression has type `Int -> Int`.

On the other hand, we don't *always* need type annotations on lambdas
like this.  For example, we can easily *check* whether `(^x -> 3)` has
a given type.  So we will make the type annotations optional.  That
is, lambda expressions will now have this syntax:

```
<expr> ::=
  ...
  | '^' <var> ('[' <type> ']')? '->' <expr>
```
Recall that `(...)?` means the part inside `(...)` is optional, *i.e.*
may occur zero or one times.

* First, write a parser for types (that is, a `Parser Type`), using
  the grammar

    ```
    <type> ::=
      | 'Int'
      | '(' <type> ')'
      | <type> '->' <type>
    ```
    Note that `->` should be right associative.

    (*Hint*: you will probably want to use a structure similar to what
    we usually do for parsing expressions, with one parser for atomic
    types and one for types in general.)


> parseAtomT :: Parser Type
> parseAtomT
>   =   It  <$ reservedOp "It"
>   <|> parens parseType


> parseType :: Parser Type
> parseType = buildExpressionParser table parseAtomT
>   where
>     table = [ [ Infix (Ft <$ reservedOp "->") AssocRight]
>             ]



* Modify the lambda constructor of `Expr` to store an optional type
  after the variable.

* Modify your expression parser to parse the new lambda syntax.  You
  will probably want to use the `optionMaybe` function.

Feedback
--------

* How long would you estimate that you spent working on this module?
3 hours
* Were any parts particularly confusing or difficult?
We were totally lost at first on ParsType, because we were thinking about it wrong
* Were any parts particularly fun or interesting?
The beginning was interesting.
* Record here any other questions, comments, or suggestions for
  improvement.
