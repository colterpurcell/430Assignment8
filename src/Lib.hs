module Lib
    (interp,
    primOpApply,
    primEnv,
    Expr(..),
    Value(..),
    Env,
    ) where
import Data.SExpresso.SExpr
import Debug.Trace (trace)

{-
OUR GUIDING LIGHT 👼 (defines what is a valid expression
‹expr› ::= ‹num›
  | ‹id› ; Ident (variable or function name) but not => with or =
  | ‹string›
  | { if ‹expr› ‹expr› ‹expr› }
  | { with ‹clause›* ‹expr› } ; introduces variable bindings
  | { ‹id›* => ‹expr› } ; function with 0 or more params e.g. {x y => {+ x y}} or {=> 42}
  | { ‹expr› ‹expr›* } ; function application: first expr is function being applied, rest are args


‹clause› ::= [ ‹id› = ‹expr› ]
-}

data Expr
  = NumE Double
  | StrE String
  | BoolE Bool
  | IdE String
  | IfE Expr Expr Expr
  | WithE [(String, Expr)] Expr
  | LamE [String] Expr
  | AppE Expr [Expr]
  | SeqE [Expr]
  deriving (Show, Eq)

data Value
  = NumV Double
  | StrV String
  | BoolV Bool
  | CloV [String] Expr Env
  | PrimV String
  | NullV -- represents a null value, similar to None in Python or null in JavaScript
  deriving (Show, Eq)

type Env = [(String, Value)]

-- | primordial environment
primEnv :: Env
primEnv =
  [ ("and", PrimV "and")
  , ("or", PrimV "or")
  , ("not", PrimV "not")
  , ("substring", PrimV "substring")
  , ("strlen", PrimV "strlen")
  , ("equal?", PrimV "equal?")
  , ("error", PrimV "error")
  , ("true", BoolV True)
  , ("false", BoolV False)
  , ("+", PrimV "+")
  , ("-", PrimV "-")
  , ("/", PrimV "/")
  , ("*", PrimV "*")
  , ("<", PrimV "<")
  , ("<=", PrimV "<=")
  , (">", PrimV ">")
  , (">=", PrimV ">=")
  , ("++", PrimV "++")
  , ("println", PrimV "println")
  , ("read-num", PrimV "read-num")
  , ("read-str", PrimV "read-str")
  ]

-- | a function to interprest abstract syntax tree (AST) expressions into values
interp :: Expr -> Env -> Value
interp (NumE n) _ = NumV n
interp (StrE s) _ = StrV s
interp (BoolE b) _ = BoolV b
interp (IdE x) env = case lookup x env of
  Just v  -> v
  Nothing -> error $ "unbound identifier: " ++ x
interp (IfE c t e) env =
  case interp c env of
    BoolV True  -> interp t env
    BoolV False -> interp e env
    v           -> error $ "if: expected a boolean, got " ++ show v
interp (WithE bindings body) env =
  let newEnv = [(x, interp e env) | (x, e) <- bindings] ++ env
  in interp body newEnv
interp (LamE args body) env = CloV args body env
interp (AppE f args) env =
  case interp f env of
    CloV params body closureEnv ->
      if length params == length args
        then interp body (zip params (map (`interp` env) args) ++ closureEnv)
        else error "wrong arity"
    PrimV name -> primOpApply name (map (`interp` env) args)
    v -> error $ "tried to apply a non-function: " ++ show v
interp (SeqE exprs) env = foldl (\_ e -> interp e env) (BoolV True) exprs

-- | a function to apply primitive operations
primOpApply :: String -> [Value] -> Value
primOpApply "+" [NumV x, NumV y] = NumV (x + y)
primOpApply "-" [NumV x, NumV y] = NumV (x - y)
primOpApply "*" [NumV x, NumV y] = NumV (x * y)
primOpApply "/" [NumV x, NumV y] = if y == 0 then error "division by zero" else NumV (x / y)
primOpApply "<=" [NumV x, NumV y] = BoolV (x <= y)
primOpApply "equal?" [x, y] = BoolV (x == y)
primOpApply "and" [BoolV x, BoolV y] = BoolV (x && y)
primOpApply "or" [BoolV x, BoolV y] = BoolV (x || y)
primOpApply "not" [BoolV x] = BoolV (not x)
primOpApply "substring" [StrV s, NumV start, NumV len] =
  let start' = round start
      len' = round len
  in StrV (take len' (drop start' s))
primOpApply "strlen" [StrV s] = NumV (fromIntegral (length s))
primOpApply "println" [StrV s] = trace s NullV
primOpApply "read-num" [] = error "read-num cannot be used as a pure function; it requires IO"
primOpApply "read-str" [] = error "read-str cannot be used as a pure function; it requires IO"
primOpApply "error" [StrV msg] = error msg
primOpApply op args = error $ "unknown primitive operation: " ++ op ++ " with args: " ++ show args


-- | a function to parse Sexpresso.Sexprs into Expr data type
{-parse :: SExpr -> Expr
parse (SAtom (Number n)) = NumE n
parse (SAtom (String s)) = StrE s
parse (SAtom (Ident x)) = IdE x
parse (SAtom (Bool b)) = BoolE b
parse (SList [SAtom (Ident "if"), c, t, e]) = IfE (parse c) (parse t) (parse e)
parse (SList (SAtom (Ident "with") : rest)) =
  case unsnoc rest of
    Just (clauses, body) ->
      WithE [(x, parse e) | SList [SAtom (Ident x), e] <- clauses] (parse body)
    Nothing -> error "Malformed with expression"
  where
    -- unsnoc splits a list into (init, last)
    unsnoc [] = Nothing
    unsnoc xs = Just (init xs, last xs)
parse (SList (SAtom (Ident "=>") : args)) =
  let (params, body) = case args of
        [] -> ([], NullV) -- handle empty function case
        _  -> (init args, last args)
  in LamE [case a of SAtom (Ident x) -> x; _ -> error "expected identifier"] (parse body)
parse (SList (f : args)) =
  let func = parse f
      args' = map parse args
  in AppE func args'
parse (SList exprs) = SeqE (map parse exprs)
-}

-- | a helper function to turn the initial user string into a Sexpr
{-parseExpr :: String -> SExpr
parseExpr input =
  case parseSExpFromString input of
    Left err -> error $ "Parse error: " ++ show err
    -}