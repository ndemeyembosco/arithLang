Module 14: Type checking first-class functions
==============================================

* Write your team names here:

Bosco Ndemeye
Joseph Matson

Source 1: Dr. Yorgey
Source 2: http://www.scs.stanford.edu/11au-cs240h/notes/ghc.html

In this module we will continue exploring how to type check a language
with first-class functions.  This language, together with its type
system, is known as the *simply-typed lambda calculus*, or
STLC^[I always get STLC and SLTC mixed up.] (extended with integers
and addition).

* Pick an initial driver and write their name here:

Bosco

* You should begin by copying in your code from the previous module,
  including the interpreter and the parser extended with optional type
  annotations on lambda expressions.

> {-# LANGUAGE GADTs #-}
>
> import Parsing2
> import qualified Data.Map as M
> import Data.Maybe
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
>   <|> Func <$>  (reservedOp "^" *> identifier) <*> (try (reservedOp "[" *> optionMaybe parseType <* reservedOp "]") <|> (Nothing <$ whiteSpace)) <*> (reservedOp "->" *> parseExpr)
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



> data Type where
>     It :: Type
>     Ft :: Type -> Type -> Type
>     deriving (Show, Eq)

> parseAtomT :: Parser Type
> parseAtomT
>   =   It  <$ reservedOp "Int"
>   <|> parens parseType


> parseType :: Parser Type
> parseType = buildExpressionParser table parseAtomT
>   where
>     table = [ [ Infix (Ft <$ reservedOp "->") AssocRight]
>             ]


Types for lambdas and applications
----------------------------------

You already know how to typecheck numeric literals, arithmetic
operations such as addition, and variables; the things we need to
focus on are the language features specifically having to do with
functions, namely, lambdas and applications. Let's think more
carefully about how to give these things types. In answering the
following questions you may want to refer back to the examples in the
previous module for inspiration.

In the following, the letter `e` will stand for any expression, and
`T` will stand for any type.

* For a function application `e1 e2` to be well-typed,
    + what must be true about the type of `e1`? It should be type e2 -> T
    + what must be true about the type of `e2`? It should have the same type as the argument to e1.
    + In general, given `e1 e2`, how can you figure out what type it
      should have? Get the type of e2 and then get the type of the output of e1.

* Consider the lambda expression `^x [T] -> e`, where the variable `x`
  is annotated with a type `T`, and `e` is some expression.  What type
  should it have? the type is T -> type of e.

* Consider a lambda expression `^x -> e` (without a type annotation on
  `x`).  Suppose someone tells you that this lambda expression is
  supposed to have a particular type `T`.  Let's try a few examples
  first:
    + Does `^x -> 3` have type `Int`? No.
    + Does `^x -> 3` have type `(Int -> Int) -> Int`? yes it might.
    + Does `^x -> x + 2` have type `(Int -> Int) -> Int`? No.
    + Does `^x -> (^y -> 3)` have type `Int -> (Int -> Int)`?Yes it might.

* Now generalize the previous examples.  How can you check whether a
  lambda expression has a given type?

  Given a lambda expression ^x -> e1 and a type T1 -> T2, check
  putting something of type T1 into e1 will evaluate. confirm that it evaluates to
  something of type T2.

So we know how to *infer* the type of an application or a lambda with
a type annotation, and how to *check* the type of a lambda without a
type annotation.

What about *inferring* the type of a lambda without a type annotation?
As you saw in the previous module, this can sometimes be done, but it
is tricky.  For example, it is possible to infer that `(^x -> (^f -> f
x + x))` has type `Int -> (Int -> Int) -> Int`, but this requires some
nontrivial deduction. For the purposes of this module, we are *not*
going to do this.  Instead, we will just throw up our hands and refuse
to infer the type of a lambda without a type annotation. (However, if
you are interested to know how this "nontrivial deduction" (called
*type reconstruction*) works in general, I would be happy to explain
it.  In fact, implementing this for some nontrivial language could
be a nice part of a final project.)

![](../images/stop.gif)

A type checker for STLC
-----------------------

* **ROTATE ROLES** and write the name of the new driver here:
  Joseph
* Use the above insights to implement a type checker.  Some notes:

> type Ctx = M.Map String Type

> data TypeError where
>   UnboundVar :: TypeError
>   TypeErr    :: Expr -> Type -> Type -> TypeError
>   InferErr   :: Expr -> TypeError
>   FuncErr    :: Expr -> TypeError
>   deriving Show

> showError ::  TypeError -> String
> showError UnboundVar = "Error: Unbound variable"
> showError (TypeErr e t1 t2) = "Error: " ++ show e ++ " should be " ++ show t1 ++ " but is " ++ show t2
> showError (InferErr e) = "Error: Expression is not a valid input: " ++ show e
> showError (FuncErr e) = "Error: this expression is not a function: " ++ show e

> infer :: Ctx -> Expr -> Either TypeError Type
> infer c (V s) = case M.lookup s c of
>     Just n -> Right n
>     Nothing -> Left UnboundVar
> infer c (I n) = Right It
> infer c (Sum m n) = (check c m It) *> (check c m It) *> Right It
> infer c (Func s t e) = case t of
>     Just t1 -> infer (M.insert s t1 c) e >>= \t2 -> Right (Ft t1 t2)
>     _ -> Left (InferErr (Func s t e))
> infer c (App e1 e2) = infer c e1 >>= \t -> case t of
>     Ft n m -> check c e2 n >>= \k -> Right m
>     It -> Left (FuncErr e1)

> check :: Ctx -> Expr -> Type -> Either TypeError ()
> check c e t = case t of
>     (Ft t1 t2)  -> case e of
>          Func s Nothing e1 -> check (M.insert s t1 c) e1 t2
>          _                 -> infer c e >>= \t1 -> case t1 of
>                                  k -> if (k == t) then (Right ()) else (Left (TypeErr e t t1))
>     _           -> infer c e >>= \t1 -> case t1 of
>                        k -> if (k == t) then (Right ()) else (Left (TypeErr e t t1))



    + Again, you should write two mutually recursive functions,
        ```
        infer :: Ctx -> Expr -> Either TypeError Type
        ```
        and
        ```
        check :: Ctx -> Expr -> Type -> Either TypeError ()
        ```
        (your functions might have
        slightly different types depending on how you decide to set
        things up).

    + In previous type checkers we have written, the `check` function
      was fairly trivial: all it did was `infer` the type of the given
      expression and make sure it matched the given type.  This time,
      however, `check` will be a bit more interesting: it should first
      see if it is given a lambda with no type annotation, and do
      something appropriate.  In all other cases, it should fall back
      to `infer`ring the type and making sure it matches the given
      type, as before.

        (As an aside, the reason we structured our previous type
        checkers using both an `infer` function and a `check` function
        was for exactly this reason: more complex languages tend to
        have some expressions whose types can be *checked* but not
        *inferred*, so both the `check` and `infer` functions do
        nontrivial work.  Structuring a type system in this way, with
        mutually recursive `check` and `infer` functions, is called
        *bidirectional type checking*.)

    + You will need to create a `TypeError` type and populate it with
      appropriate constructors representing the various possible sorts
      of errors your type checker may encounter.

![](../images/green.png)

Updating the interpreter
------------------------

* **ROTATE ROLES** and write the name of the new driver here:

Bosco

> interpC :: Env -> Expr -> Value
> interpC e (V s) = fromJust $ (M.lookup s e)
> interpC e (I n) = In n
> interpC e (Sum e1 e2) = case (interpC e e1, interpC e e2) of
>         (In n, In m) -> In (n + m)
>         _           -> error "Impossible TypeError!"
> interpC e (App e1 e2) = case (interpC e e1, interpC e e2) of
>            (Fu s ex env, n) -> interpC (M.insert s n env) ex
>            _                -> error "Impossible FuncError"
> interpC e (Func s _ m) = Fu s m e

> eval :: String -> IO ()
> eval s = case parse expr s of
>   Left _    -> putStrLn "SyntaxError"
>   Right a   -> case infer M.empty a of
>       Left n    -> putStrLn (showError n)
>       Right b   -> putStrLn (showValue $ interpC M.empty a)



  > eval :: String -> IO ()
  > eval s = parse expr s >>= \a -> infer M.empty a >>= \b -> interpC M.empty a >>= \c -> case c of
  >     k  -> print k


Now that we have a type checker, the interpreter doesn't have to do so
much work.  However, *unlike* with previous interpreters, we can't
really get rid of the `Value` type.  When we interpreted Arith with
booleans and integers, for example, we could just represent all values
as integers; but now we have integers and closures, which are so
different that there's no appropriate type we can use to easily
represent both.  So your interpreter will still need to pattern-match
on `Value`s. Nonetheless, it can still be simplified quite a bit:

* If an expression type checks, interpreting it should *never* cause a
  runtime error: type errors and unbound variables are of course
  impossible, and this language doesn't have division by zero either.
  So you should change the type of your interpreter to just return a
  `Value` instead of an `Either InterpError Value`.

* If you see, for example, a `Plus` operation, you can assume that the
  `Value`s of the two subexpressions will be integers.  You do not
  even need to include cases for the other possibilities (or, if you
  like, you can have a catch-all case that simply calls `error` with a
  panicked message exhorting the user to report a bug in your type
  checker).

* Finally, if you have not already, update an appropriate `eval ::
  String -> IO ()` function which accepts an expression as input and
  prints either a parse error, a type error, or the final value
  resulting from interpreting a correctly type-checked expression.


  * Here are some examples you can use to test your implementation:

      - `3` should evaluate to 3.
      - `4 5` should be a type error.
      - `(^x -> 3) 4` should be a type error, since we can't infer the
        type of the lambda.
      - `(^x [Int] -> 3) 4` should evaluate to 3.
      - `(^x [Int -> Int] -> 3) 4` should be a type error.
      - `x + 3` should give an *undefined variable error* (not a type error).
      - `(^f [Int] -> f x + f 2) 5` should give an undefined variable
        error or a type error (either one is OK).
      - `(^f [Int] -> f 1 + f 2) 5` should be a type error.
      - `(^f [Int -> Int] -> f 1 + f 2) 5` should be a type error.
      - `(^f [Int -> Int] -> f 1 + f 2) (^x [Int] -> x + 8)` should evaluate to 24.
      - `(^f [Int -> Int] -> f 1 + f 2) (^x -> x + 8)` should also evaluate to 24.
        It should *not* be a type error.
      - `(^f : (Int -> Int) -> (Int -> Int). f (^y. y + 10) 6) (^g. ^x. g (g (g x)))`
        should evaluate to 36.

  ![](../images/green.png)

  Correcting a few issues
  -----------------------

  * **ROTATE ROLES** and write the name of the new driver here:

  * What is the type of `show`?

   a -> String

  * What is the result of `show "hello"`?
  "\"hello\""
  * What is the result of `show (show "hello")`?
  "\"\\\"hello\\\"\""
  * Can you explain what is going on in the above results?
    show takes the data out of it's own type and puts it in String type, so
    if it is already a string then it gets ugly.
  * What is the type of `putStrLn`?
    String -> IO ()
  * What is the type of `print`?
    a -> IO ()
  * Just by looking at their types, make a guess as to the difference
    between these two functions.
    putStrLn is used only for strings, while print can be used for any type with a Show instance.
  * Test your guess by trying things like
      + `putStrLn 3`
      + `putStrLn (show 3)`
      + `print 3`
      + `putStrLn "hello"`
      + `putStrLn (show "hello")`
      + `print "hello"`

  * Explain when you should use `print` and when you should use `putStrLn`.
      you should putStrLn only when dealing with strings otherwise use print.
  * Explain what is wrong with this hypothetical code:

      ```{.haskell}
      case interp M.empty e of
        Right v  -> print (showValue v)
        Left err -> print (showError err)
      ```

  * How would you fix it?

  we should use putStrLn instead of print since both showValue and showError output a String.

  * Explain what is wrong with this hypothetical code:

      ```{.haskell}
      case interp env e of
        Right (IntValue i) -> doStuff i
        _                  -> Left $ NotAnIntError e
      ```
      This is wrong because it doesn't cover all cases. A function type should not be an error.

  * How would you fix it?

    Add another case for Function types.

  * Go back and make sure your module is free of these issues.

  ![](../images/green.png)

  Lambda calculus in the real world
  ---------------------------------

  Take a look at
  [this page from the GHC wiki](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType). Haskell
  syntax is complicated, but it all ultimately gets desugared to this
  very simple data type `Expr`.

  * Which parts of `Expr` look familiar?  Which are unfamiliar?
    Most of it looks familiar, like let statements and lambda expressions and applications, but
    "Case" "Cast" and "Tick" look unfamiliar.
  * Go learn something about one of the unfamiliar parts, and explain it
    here.
    Tick is used for supporting "source annotation," it can track the evaluations of different expressions.

  * Take a look at
    [this function](https://ghc.haskell.org/trac/ghc/browser/ghc/compiler/coreSyn/CoreLint.hs#L614)
    from the GHC source. You won't understand a lot of it, but that's
    OK.  What is this function doing?

    it seems to be taking an expression and returning the type

  Feedback
  --------

  * How long would you estimate that you spent working on this module?
        4 hours
  * Were any parts particularly confusing or difficult?
    The undefined types for functions were difficult.
  * Were any parts particularly fun or interesting?
    Seeing the underlying structure of Core was really interesting.
  * Record here any other questions, comments, or suggestions for
    improvement.
