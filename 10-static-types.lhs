Module 10: Static types
=======================

This module is due at **1:15pm on Tuesday, October 11**.

* Write your team names here:

  Joseph Matson
  Bosco Ndemeye

  Source 1: Dr. Brent Yorgey for constant guidance and support
  Source 2: Module 8 and 9

In this module, we will continue our exploration of *type systems*.

Once again, we will build on the Arith language from previous
weeks.

* Choose any driver to start.

  Joseph

* Begin by copying **only your AST types and parser** (and desugaring,
  if applicable) from module 09 below.  Do **not** copy your
  interpreter, since we will not be extending it, but instead redoing
  it in a rather different style.


> {-# LANGUAGE GADTs #-}
>
> module ArithVars where
>
> import qualified Data.Map as M
> import           Parsing  hiding ((<$), (<$>), (<*>))
> import           Prelude
>
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
>
> arith :: Parser Arith
> arith = whiteSpace *> parseArith <* eof


Static type checking
====================

We are going to add a new **type checking** phase in between parsing
and interpreting. (**Note**, if you did the optional extension in the
previous module to add extra operators like `&&`, `>=`, and so on,
read this
footnote.^[If you desugar the new operators directly inside the parser,
you will necessarily be running the type checker over the simpler, desugared syntax,
 which means that any type errors will refer to desugared programs. This is confusing
 for the programmer, because they may get errors about programs they did not write.
  For example, if the programmer writes `True && 3`, it will first desugar to
   `if True then 3 else False`, and they will then get a type error referring to this
   `if`-expression even though they never wrote any `if`.  This is one good reason to
   have desugaring as a separate phase from parsing: first, the more complex language
    (the "surface language") is parsed into an AST which can represent the complete
    syntax of the surface language; this is then typechecked, so any type errors refer
to the actual program the programmer wrote; a desugaring function then translates the
successfully typechecked AST into the simpler language (the "core language"); finally,
the interpreter (or compiler, optimizer, *etc*.) can work directly with the simplified
core language.  Thankfully, in this case, writing a typechecker for the language extended
with a bunch of extra operators is only slightly more work than writing a typechecker for
the simpler language. The hard parts are things like `if` and `let`; adding more operators is relatively trivial.])
The type checker should adhere to the **Central Dogma of
Typechecking**:

**If type checking succeeds, then there should be no runtime errors**.

Or, more pithily,

**Well-typed programs don't go wrong**.^[This phrase is due to Robin Milner, though in the original
context he usese it with a precise technical meaning, not as a general guiding principle for type systems.
See Milner, Robin. "[A theory of type polymorphism in programming]
(http://www.research.ed.ac.uk/portal/files/15143545/1_s2.0_0022000078900144_main.pdf)." Journal of computer and system sciences 17.3 (1978): 348-375.]

(We will add some caveats to this later.)

* First, we will need a Haskell data type to represent types in Arith.
  Create a new data type called `Type` with two constructors to
  represent the two types in Arith, namely, integers and booleans.

> data Type where
>   I :: Type
>   B :: Type
>   deriving (Eq, Show)

* We will also need a data type to represent errors that can be
  generated during type checking.  For now, it should have two
  constructors:

> data TypeError where
>   UnboundVar :: TypeError
>   TypeErr    :: Arith -> Type -> Type -> TypeError
>   deriving Show

> showTypeError :: TypeError -> String
> showTypeError UnboundVar = "Sorry variable is undefined"
> showTypeError (TypeErr a t1 t2) = "TypeError: " ++ show a ++ " should be type " ++ show t2 ++ " but is " ++ show t1

    - One to represent the occurrence of an undefined (or "unbound")
      variable.

    - One to represent a type error.  It should contain three fields:
      an `Arith` (representing the expression which failed to
      typecheck), and two `Type`s (representing the type the
      expression was expected to have, along with its actual type).

* Remember that when writing an interpreter we need a way to deal with
  variables: when we get to a variable, how should it be interpreted?
  We solved this problem by adding an *environment* which was a
  mapping from variables to values.  We have a similar problem when
  type checking: when we get to a variable, what type should it have?
  The solution is also similar: we need a *type context* which is
  mapping from variables to *types*. Define `type Ctx = M.Map String
  Type`.

> type Ctx = M.Map String Type

* Now write the type checker itself!  You will want to implement two
  main functions, though you may also want to add other helper
  functions as well. (**Hint**: be sure to read all the "notes" below
  *before* starting to implement these functions!)

    - `infer :: Ctx -> Arith -> Either TypeError Type` takes a typing
      context and an `Arith` expression, and *infers* the type of the
      expression (or generates an error if the expression has no
      type).

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

    - `check :: Ctx -> Arith -> Type -> Either TypeError ()` takes a
      typing context, an `Arith` expression, and a type, and *checks*
      that the expression has the given type.  It throws an error if
      either the expression does not have a type, or if it has a type
      which is different than the given one.  Note that it returns a
      value of type `()` (unit), which carries no useful information:
      we only care whether there are any errors or not.

    Some notes:

    - `check` should be very easy to implement: just `infer` the type
      of the given `Arith` expression, and then test whether its
      inferred type is equal to the given type.

    - `infer` can make use of `check` in situations where you know some
      `Arith` expression needs to have a certain type.  For example, if
      you encounter an expression `a + b`, you should `check` that both
      `a` and `b` have type Integer.

    - Yes, you read that right: `check` and `infer` should be *mutually
      recursive*, that is, each calls the other.

    - Remember that for parsers, `p1 *> p2` runs parser `p1` and then
        throws away its input before running `p2`.  In this context, if
        `t1` and `t2` are expressions that could throw a type checking
        error, `t1 *> t2` will run `t1` and make sure it does not generate
        an error before running `t2`.  This is especially useful for
        calling `check` (which does not return a useful result anyway).
        For example, you might have something like this to infer the type
        of `a + b`:
        ```
        infer ctx (Bin Add a b) =
          check ctx a TyInt *>       -- make sure a has type Int
          check ctx b TyInt *>       -- make sure b has type Int
          Right TyInt                -- If no errors were generated,
                                     --   finally return Int as the type
                                     --   of the whole expression (a + b)
        ```

    - `infer` will also need to make use of `(>>=)`.  For example,
      when inferring the type of an `if` expression, you will need to
      infer the types of its `then`- and `else`-parts:
        ```
        infer ctx (If test t1 t2) =
          ... other stuff ...
          infer ctx t1 >>= \ty1 ->
          ... some more stuff here that can use ty1 ...
        ```

    - The types of all the binary operators should be evident.
      *Hint*: You might find it useful to make a function `bopType ::
      Op -> (Type, Type, Type)` which returns the expected input types
      and the output type of a binary operator.

    - The most interesting cases are how to typecheck `if`-expressions
      and `let`-expressions.  Rather than tell you how they are done,
      I will let you think about them---in principle, you should be
      able to derive the correct implementation by thinking about how
      these expressions are interpreted. But feel free to ask me
      questions.

* Finally, make a function `inferArith :: Arith -> Either TypeError
  Type` which simply runs `infer` starting with an empty typing
  context.

> inferArith :: Arith -> Either TypeError Type
> inferArith n = infer M.empty n

![](../images/stop.gif)

Interpreter
===========

* **ROTATE ROLES** and write the name of the new driver here:

    Bosco

* After typechecking has succeeded, can we still get runtime errors?
  If so, which runtime errors can still happen?  Which are no longer
  possible?

 We can still run into runtime errors like dividing by zero, but we can no longer run
 into undefined variable errors.


* As you can see, the Central Dogma of Typechecking does not hold
  literally in our case!  In general, we pick a subset of runtime
  errors that we care about, and design a type system which prevents
  *those particular* runtime errors, but may not prevent others.

Given a static type checker, we can do something very interesting with
the interpreter.  The whole idea of static type checking is that if a
program successfully type checks, there will never be any type errors
at runtime, so checking for them would be a complete waste of time.

One might think, however, that this runtime type checking is
unavoidable: our programs can evaluate either to booleans or integers,
so we need the `Value` type to represent the two possibilities.  Then
we have to pattern-match on `Value`s all the time to make sure we have
the right kind of value (even though we know that one of the cases
cannot happen).

However, we can use a trick: we can make the interpreter just return
`Integer` again, and *encode* boolean values as integers. For example,
encode `False` as 0, and `True` as 1.  When the interpreter is
running, there is no way to tell whether a particular `Integer` value
is supposed to represent an actual integer or a boolean---we say that
the type information has been *erased*.  This means that *in theory*,
the interpreter could perform nonsensical operations such as adding
`3 + True` (resulting in `4`).  However, the interpreter will only
ever run programs which successfully typecheck; if the typechecker is
implemented correctly, then cases like this can never actually happen!
Such errors would have been caught as type errors. (Ideally, this is
something we could *formally prove* about our system.)  This is one
reason static type checking can lead to better runtime performance: we
don't have to carry around type information and constantly check types
of values at runtime, which can remove a lot of runtime overhead (as
well as create better opportunities for optimization).

* Begin by copying your interpreter from **module 8** and pasting it
  here (*not* your dynamic typechecking interpreter from the previous
  module).

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
> --interpArith (Bin Div _ (Lit 0)) e  = Left DivisionByZero
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

* Remove any `InterpError`s which cannot happen anymore.  Replace them
  in the interpreter with calls to `error`, with a message like `"Bug!
  This case should be impossible!  Fizzwoz cannot happen in the
  interpreter, since bar baz."` (of course, you should replace the
  nonsense with something appropriate).

* Now extend `interpArith` appropriately.  Remember that in the case
  that you need a boolean value, you can just *assume* that you have
  an `Integer` which is either `0` or `1` representing `False` or
  `True`.

* Finally, update the `eval` function.  It should deal appropriately with
  parse errors, type errors, runtime errors, and with showing the
  output of the interpreter.  For example, here is what my solution
  looks like (note that depending on whether you did the optional
  extension, yours may not be able to handle things like `||`, `<=`,
  and so on):

> eval :: String -> IO ()
> eval s = case parse arith s of
>   Left m -> print m
>   Right e -> case inferArith e of
>       Left t -> print (showTypeError t)
>       Right t -> case interpArith e M.empty of
>                    Right n -> print n
>                    Left err       -> putStrLn (showInterpError err)

    ```
    λ> eval "let y = 2 in let x = (if (3 > 5) || (y*3 <= 10) then 5+y else 9*10) in x"
    7
    λ> eval "let x = 3 in ! (x < 8)"
    False
    λ> eval "let y = 2 in let x = (if (3 > 5) || (y*3 <= 10) then 5+y else 9*10) in z"
    Unbound variable z
    λ> eval "let y = False in let x = (if (3 > 5) || (y*3 <= 10) then 5+y else 9*10) in x"
    Type mismatch: Var "y" was used in a context that expected TInt,
      but it actually has type TBool.
    λ> eval "let y = 2 in let x = (if (3 > 5) || (y*3 <= 10) then 5+y else False && True) in x"
    The branches of the if expression If (Bin FOr (Bin FGt (ILit 3) (ILit 5)) (Bin FLeq (Bin FTimes (Var "y") (ILit 3)) (ILit 10)))
      (Bin FPlus (ILit 5) (Var "y"))
      (Bin FAnd (BLit False) (BLit True))
      should have the same type, but they have types TInt and TBool respectively.
    ```

    A few notes:

    - To get really nice type error messages, one would have to
      include a pretty-printer.  As you can see, I have not done so,
      but you are welcome to add one if you wish.
    - As you can see, I included a special kind of type error for
      `if`-expressions; you can do this if you wish but it is
      optional.
    - Note that one cannot simply `show` the result of the
      interpreter, since this would sometimes *e.g.* show `0` when it
      should be `False`.  To correctly show the output of the
      interpreter, one needs to take the inferred type of the
      expression into account.

* Try evaluating `if False then 3 else True`.  What happens?  How is
  this different than the previous module?

Feedback
--------

* How long would you estimate that you spent working on this module?

4 hours

* Were any parts particularly confusing or difficult?

No

* Were any parts particularly fun or interesting?

Everything!

* Record here any other questions, comments, or suggestions for
  improvement.
