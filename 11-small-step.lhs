Module 11: Small-step reduction
===============================

This module is due at **1:15pm on Tuesday, October 18**.

* Write your team names here:

  Bosco Ndemeye, Collin Shaddox, Joseph Matson

* Pick an initial driver and write their name here:
    Joseph

Source 1: https://www.haskell.org/hoogle/?hoogle=%3C%7C%3E
Source 2: http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html#v:-60--124--62-
Source 3: CSCI 490, Dr. Yorgey
* Begin by pasting in your most recent implementation of the Arith
  language (or you can again
  [use mine](../code/ArithVars.lhs), although using a version with a
  working type checker may be nicer (though not required)).


> {-# LANGUAGE GADTs #-}
>
> module ArithVars where
>
> import qualified Data.Map as M
> import           Parsing  hiding ((<$), (<$>), (<*>), (<|>))
> import           Control.Applicative
> import           Prelude
> import qualified Data.Set as S
> -- Abstract syntax
>
> data Arith where
>   Lit :: Integer -> Arith
>   Bin :: Op -> Arith -> Arith -> Arith
>   Var :: String -> Arith
>   Let :: String -> Arith -> Arith -> Arith
>   If :: Arith -> Arith -> Arith -> Arith
>   Bool :: Bool -> Arith
>   deriving (Show)
>
> data Op where
>   Plus  :: Op
>   Minus :: Op
>   Times :: Op
>   Div   :: Op
>   Less  :: Op
>   Great :: Op
>   Equal :: Op
>   deriving (Show, Eq)
>
> -- Parser

>
> lexer :: TokenParser u
> lexer = makeTokenParser $ emptyDef
>   { reservedNames = ["let", "in", "True", "False", "if", "else", "then"] }
>
> parens :: Parser a -> Parser a
> parens     = getParens lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer
>
> reserved :: String -> Parser ()
> reserved   = getReserved lexer
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
> parseArithAtom =
>       Lit <$> integer
>   <|> Var <$> identifier
>   <|> parseLet
>   <|> parens parseArith
>   <|> parseIf
>   <|> Bool <$> ((True <$ reserved "True") <|> (False <$ reserved "False"))
>
> parseIf :: Parser Arith
> parseIf = If
>   <$> (reserved   "if"     *> parseArith)
>   <*> (reserved   "then"   *> parseArith)
>   <*> (reserved   "else"   *> parseArith)
>
>
> parseLet :: Parser Arith
> parseLet = Let
>   <$> (reserved   "let" *> identifier)
>   <*> (reservedOp "="   *> parseArith)
>   <*> (reserved   "in"  *> parseArith)
>
> parseArith :: Parser Arith
> parseArith = buildExpressionParser table parseArithAtom
>   where
>     table = [ [ Infix (Bin Times <$ reservedOp "*") AssocLeft
>               , Infix (Bin Div   <$ reservedOp "/") AssocLeft
>               ]
>             , [ Infix (Bin Plus  <$ reservedOp "+") AssocLeft
>               , Infix (Bin Minus <$ reservedOp "-") AssocLeft
>               ]
>             , [ Infix (Bin Less <$ reservedOp "<" ) AssocNone,
>                 Infix (Bin Great <$ reservedOp ">" ) AssocNone,
>                 Infix (Bin Equal <$ reservedOp "==" ) AssocNone
>               ]
>             ]
> arith :: Parser Arith
> arith = whiteSpace *> parseArith <* eof

> data Type where
>   I :: Type
>   B :: Type
>   deriving (Eq, Show)

> data TypeError where
>   UnboundVar :: TypeError
>   TypeErr    :: Arith -> Type -> Type -> TypeError
>   deriving Show

> showTypeError :: TypeError -> String
> showTypeError UnboundVar = "Sorry variable is undefined"
> showTypeError (TypeErr a t1 t2) = "TypeError: " ++ show a ++ " should be type " ++ show t2 ++ " but is " ++ show t1

> type Ctx = M.Map String Type
> infer :: Ctx -> Arith -> Either TypeError Type
> infer c (Lit n) = Right I
> infer c (Bool n) = Right B
> infer c (Var v) = case M.lookup v c of
>         Just n -> Right n
>         _      -> Left (UnboundVar)
> infer c (Bin op n m) = case op of
>    Plus -> check c n I *> check c m I *> Right I
>    Minus -> check c n I *> check c m I *> Right I
>    Times -> check c n I *> check c m I *> Right I
>    Div -> check c n I *> check c m I *> Right I
>    Equal -> infer c n >>= \v -> check c m v *> Right B
>    Less -> check c n I *> check c m I *> Right B
>    Great -> check c n I *> check c m I *> Right B
> infer c (Let s n m) = (infer c n) >>= \v -> infer (M.insert s v c) m
> infer c (If n m o) = case infer c n of
>    Right B -> infer c m >>= \v1 -> check c o v1 *> infer c m
>    otherwise -> infer c n >>= \v1 ->  Left (TypeErr n v1 B)

> check :: Ctx -> Arith -> Type -> Either TypeError ()
> check c a t = case infer c a of
>       Right n -> if (n == t) then (Right ()) else (Left (TypeErr a n t))
>       Left e -> Left e

> inferArith :: Arith -> Either TypeError Type
> inferArith n = infer M.empty n

> type Env = M.Map String Integer
> data InterpError where
>     UndefinedVar :: String -> InterpError
>     DivisionByZero :: InterpError
>     deriving Show

> showInterpError :: InterpError -> String
> showInterpError DivisionByZero = "error: Division by zero"



> interpBool :: Bool -> Integer
> interpBool True = 1
> interpBool False = 0

> interpArith :: Arith -> Env -> Either InterpError Integer
> interpArith (Lit i)  e         = Right i
> interpArith (Bool True) e      = Right 1
> interpArith (Bool False) e      = Right 0
> interpArith (Var s) e = case M.lookup s e of
>                                 Just n -> Right n
>                                 _ -> error ("bug!! this case is IMPOSSIBLE! " ++ show s ++ " wouldn't typecheck! ")
> interpArith (Bin Plus e1 e2) e  = (+) <$> (interpArith e1 e) <*> (interpArith e2 e)
> interpArith (Bin Minus e1 e2) e = (-) <$> (interpArith e1 e) <*> (interpArith e2 e)
> interpArith (Bin Times e1 e2) e = (*) <$> (interpArith e1 e) <*> (interpArith e2 e)
> interpArith (Bin Great e1 e2) e = (\ a b -> interpBool (a > b)) <$> (interpArith e1 e) <*> (interpArith e2 e)
> interpArith (Bin Less e1 e2) e = (\ a b -> interpBool (a < b)) <$> (interpArith e1 e) <*> (interpArith e2 e)
> interpArith (Bin Equal e1 e2) e = (\ a b -> interpBool (a == b)) <$> (interpArith e1 e) <*> (interpArith e2 e)


> interpArith (Bin Div e1 e2) e   = case interpArith e2 e of
>                                          (Right 0) -> Left DivisionByZero
>                                          _         -> (div) <$> (interpArith e1 e) <*> (interpArith e2 e)
> interpArith (Let s v a) e       = (interpArith v e) >>= (\m -> interpArith a (M.insert s m e))
> interpArith (If e1 e2 e3) e     = case interpArith e1 e of
>      Right 0 -> interpArith e3 e
>      Right 1 -> interpArith e2 e
>      Left n  -> Left n

> eval :: String -> IO ()
> eval s = case parse arith s of
>   Left m -> print m
>   Right e -> case inferArith e of
>       Left t -> print (showTypeError t)
>       Right t -> case interpArith e M.empty of
>                    Right n -> print n
>                    Left err       -> putStrLn (showInterpError err)


* I promise that after fall break we will be done with Arith. ;-)

Bound variables
---------------

Consider the expression `let x = e1 in e2`.  We say that this
expression *binds* the variable `x`, that is, it introduces a local
definition for `x`.  We say that `x` is *bound* in `e2`, that is, any
occurrences of `x` in `e2` refer to the `x` defined by the `let`.
(However, `x` is *not* bound in `e1`---we will think more about this
later.)

For example, in
```
let x = 2+3 in x + if x < 10 then 3 else 7
```
both occurrences of `x` after the `in` refer to the `x` defined by the
`let`, so this expression evaluates to 8.

Actually, it is not quite true that *any* occurrence of `x` in `e2`
refers to the `x` defined by the `let`.  We have to take *shadowing*
into account.  For example, consider this expression:
```
let x = 5 in let x = 6 in x + x
```

* What does this evaluate to, 10 or 12?
  12
* Why?
  Because M.insert replaces a value in the Map.

* What does this expression evaluate to (be sure to guess before trying it)?
    ```
    let x = 2+3 in let x = 2*x in 4+x
    ```
    (*Hint*: remember that in `let x = e1 in e2`, `x` is bound in `e2`
    but *not* in `e1`.)
    it evaluates to 14.
* What would it mean if `let x = e1 in e2` bound `x` in *both* `e1`
  and `e2`?
  The statement would recursively loop indefinitely.

* Do you think this could be a useful feature for the Arith language?
    Not really, it seems like it would only cause problems.

* Try evaluating `let x = 2+3 in let x = 2*x in 4+x` as a *Haskell*
  expression (that is, just type it at a GHCi prompt).  What do you
  think this means about `let` in Haskell?

  In haskell it goes into a loop, meaning that it does bind the variable in both terms.

![](../images/stop.gif)

Free variables and substitution
-------------------------------

* **ROTATE ROLES** and write the name of the new driver here:
    Collin

A variable is *free* if it is not bound.  For example, in the
expression
```
let x = 2 in x + y
```
The `x` is bound, but `y` is free.

* Write a function `fv :: Arith -> S.Set String` which computes the
  set of free variables of an `Arith` expression.  You should add
    ```
    import qualified Data.Set as S
    ```
    at the top of the module; see the
    [documentation for `Data.Set` here](http://hackage.haskell.org/package/containers-0.5.8.1/docs/Data-Set.html).

    We won't actually need the `fv` function, but it is a good warm-up
    exercise for thinking about bound and free variables.

> fv :: Arith -> S.Set String
> fv (Lit _)       = S.empty
> fv (Bin _ a1 a2) = S.union (fv a1) (fv a2)
> fv (Var s)       = S.singleton s
> fv (Let s a1 a2) = S.union (fv a1) (S.delete s (fv a2))
> fv (If a1 a2 a3) = S.unions [fv a1, fv a2, fv a3]
> fv (Bool _)      = S.empty

> p :: String -> Arith
> p s = case parse arith s of
>         Left e -> error (show e)
>         Right a -> a


* Now write a *substitution* function,
    ```
    subst :: String -> Arith -> Arith -> Arith
    ```
    `subst x e1 e2` should result in `e2`, but with every *free*
    occurrence of `x` replaced by `e1`.  For example,
    ```
    subst x 3 "x + let x = x+1 in x*6"  =  "3 + let x = 3+1 in x*6"
    ```
    In the above example every free occurrence of `x` is replaced by
    `3`, but the bound occurrences of `x` are left alone. (The above
    is not actually valid syntax; I am using strings as a shorthand
    for their parsed `Arith` equivalents.  To help test `subst` and
    other functions later, it may be helpful to write a function `p ::
    String -> Arith` which tries to parse the given `String` and
    simply crashes with `error` if it does not.  Then you can actually
    write things like `subst "x" (p "3") (p "x + let x ...")`.)

> subst :: String -> Arith -> Arith -> Arith
> subst s a1 (Lit i)      = Lit i
> subst s a1 (Bool b)      = Bool b
> subst s a1 (Var i)
>    | i == s    = a1
>    | otherwise = Var i
> subst s a1 (Bin op e1 e2) = Bin op (subst s a1 e1) (subst s a1 e2)
> subst s a1 (Let s1 e1 e2)
>    | s == s1   = Let s1 (subst s a1 e1) e2
>    | otherwise = Let s1 (subst s a1 e1) (subst s a1 e2)
> subst s a1 (If e1 e2 e3) = If (subst s a1 e1) (subst s a1 e2) (subst s a1 e3)



![](../images/green.png)

Small-step reduction
--------------------

* **ROTATE ROLES** and write the name of the new driver here:

Bosco

When we have written interpreters before, they have used what is
called a *big-step semantics*: for example, they take an `Arith` and
simply reduce it to a value in one "big step".  There is no way to
extract any sort of intermediate expressions or states from the
interpreter: running the interpreter is an all-or-nothing sort of
operation.

We will now consider an alternative model, where we define what it
means for an expression to take a (small) *reduction step*, resulting
in another expression.  A reduction step changes as little as possible
while still making nontrivial progress.  We can then write an
interpreter by repeatedly applying reduction steps until the
expression cannot take any more steps.  For example, consider the
expression `if 3 + 5 < 7 then 2*5 else (let x = 3 in x+x)`.  The
`interpArith` function would simply take this expression and return
`6` (by making recursive calls to `interpArith` to evaluate the
subexpressions, and so on).  On the other hand, if we evaluate the
expression by repeatedly applying small reduction steps, it would
reduce as follows:

```
if 3 + 5 < 7 then 2*5 else (let x = 3 in x+x)
if 8 < 7 then 2*5 else (let x = 3 in x+x)
if False then 2*5 else (let x = 3 in x+x)
let x = 3 in x+x
3+3
6
```

Writing an interpreter in this style is usually inefficient compared
to a direct (big-step) implementation, but it can be a very useful way
to think and reason about the semantics of a language.  It is often
useful to define both small-step and big-step semantics for a
language, along with a formal proof that the two give equivalent
results.

![](../images/stop.gif)

* Write a function `step :: Arith -> Maybe Arith` which implements one
  step of reduction.  If the input `Arith` cannot take a step (either
  because of an error, or because there is no more reduction left to
  do), `step` returns `Nothing`.  Some notes to help you in your
  implementation are below, with keywords highlighted.  Start writing
  your implementation and consult the list of notes as you go.


> fromOp :: Op -> Arith -> Arith -> Maybe Arith
> fromOp Plus (Lit e1) (Lit e2) = Just (Lit (e1 + e2))
> fromOp Minus (Lit e1) (Lit e2) = Just (Lit (e1 - e2))
> fromOp Times (Lit e1) (Lit e2) = Just (Lit (e1 * e2))
> fromOp Div (Lit e1) (Lit e2) = Just (Lit (e1 `div` e2))
> fromOp Less (Lit e1) (Lit e2) = Just (Bool (e1 < e2))
> fromOp Great (Lit e1) (Lit e2) = Just (Bool (e1 > e2))
> fromOp Equal (Lit e1) (Lit e2) = Just (Bool (e1 == e2))
> fromOp _ _ _                   = Nothing


> step :: Arith -> Maybe Arith
> step (Lit n) = Nothing
> step (Bool b) = Nothing
> step (Var s) = Nothing
> step (Bin op e1 e2) = case step e1 of
>                  Nothing -> case step e2 of
>                       Nothing -> fromOp op e1 e2
>                       Just n  -> Just (Bin op e1 n)
>                  Just n  -> Just (Bin op n e2)
> step (Let s e1 e2) = case step e1 of
>                  Nothing -> Just (subst s e1 e2)
>                  Just n  -> Just (Let s n e2)
> step (If e1 e2 e3) = case e1 of
>             Bool True  -> Just e2
>             Bool False -> Just e3
>             _          -> step e1 >>= \v -> Just (If v e2 e3)


    * Of course, **literals** cannot take a step.

    * **Variables** cannot take a step either.  Note we are *not*
      going to pass along an environment mapping variables to values,
      as we did with `interpArith`.  A variable by itself simply
      cannot take a step.  You will see how to deal with variables
      when you get to `let`.

    * To step an application of a **binary operator**, $e_1 \mathbin{op} e_2$:
        1. First see if the left-hand argument $e_1$ can take a step
           (by calling `step` recursively).
           If $e_1 \longrightarrow e_1'$ (that is, $e_1$ can
           successfully take a step, resulting in $e_1'$), then the
           result of stepping $e_1 \mathbin{op} e_2$ is $e_1'
           \mathbin{op} e_2$.

            For example, `"(3+2) * (2*3)"` steps to `"5 *
            (2*3)"`. (Again, these strings are really just standing in
            for appropriate parsed `Arith` values.)

        2. Otherwise, if the left-hand argument cannot take a step,
           see if the right-hand argument can take a step.  For
           example, `"5 * (2*3)"` steps to `"5 * 6"`.

        3. If neither argument can take a step, then see if they are
           both literals appropriate to the operator.  If so, do the
           operation.  For example, given `"5 * 6"`, neither side can
           take a step (since they are already literals), so the whole
           expression steps to `"30"`.

        4. Remember to check for division by zero.  Instead of
           generating an error, however, an expression like `"5 / 0"` simply
           cannot take a step; it is "stuck".

        5. Otherwise (*e.g.* if you have something like `"5 + True"`),
           the operator cannot take a step.

    * I will let you think about **if** and **let** on your own.  A
      few hints:

        * Use the example reduction from above (of `if 3 + 5 < 7 then
          2*5 else (let x = 3 in x+x)`) to help guide you.

        * Remember the `subst` function!

    * Once you get to the point where you are **annoyed by
      pattern-matching on Maybe**, consider these questions:

        * What is the type of `Bin op <$> step e1 <*> Just e2`? What
          does it evaluate to?
        Maybe Arith
        It evaluates to the bin with e1 stepped if e1 can be stepped, and Nothing otherwise.


        * What is the type of
            ```
            (Bin op <$> step e1 <*> Just e2) <|> (Bin op <$> Just e1 <*> step e2)
            ```
            ?  What does it evaluate to?
        The type is Maybe Arith
        It steps e1 if possible, and if not, it steps e2.

        * Can you use these tools to write `step` without ever
          explicitly writing a `case` on the results of recursive
          calls to `step`?
        Yes.

* Now write a function `reduce :: Arith -> Arith` which simply
  iterates `step` until the expression cannot take another step, and
  returns the final, un-steppable expression.

> reduce :: Arith -> Arith
> reduce a = case step a of
>            Nothing -> a
>            Just a2 -> reduce a2

* Finally, write a function `eval2 :: String -> IO ()` which works
  similarly to `eval` but uses `reduce` instead of `interpArith`.  You
  can use this to test your implementation of `step`, to make sure
  that stepwise reduction yields the same results as `interpArith`.

> eval2 :: String -> IO ()
> eval2 s = case parse arith s of
>           Left m  -> print m
>           Right e -> print (reduce e)


Feedback
--------

* How long would you estimate that you spent working on this module?
3 hours

* Were any parts particularly confusing or difficult?
Getting <|> to work was a pain because of Parsing.(<|>).

* Were any parts particularly fun or interesting?
subst and fv were fun to write.

* Record here any other questions, comments, or suggestions for
  improvement.
