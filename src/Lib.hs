module Lib
    (interp,
    primOpApply,
    primEnv,
    serialize,
    Expr(..),
    Value(..),
    Env,
    ) where
import Debug.Trace (trace)
import Data.SExpresso.SExpr
import qualified Data.SExpresso.Parse as Parser

data Atom
  = AIdent String
  | ABool Bool
  | ANum Double
  | AStr String

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

-- | a function that takes in a Value and returns a string for tests and debugging
serialize :: Value -> String
serialize (NumV n) = show n
serialize (BoolV True) = "true"
serialize (BoolV False) = "false"
serialize (StrV s)     = show s
serialize (CloV _ _ _) = "#<procedure>"
serialize (PrimV _)    = "#<primop>"

-- | a function to apply primitive operations
primOpApply :: String -> [Value] -> Value
primOpApply "+" [NumV x, NumV y] = NumV (x + y)
primOpApply "-" [NumV x, NumV y] = NumV (x - y)
primOpApply "*" [NumV x, NumV y] = NumV (x * y)
primOpApply "/" [NumV x, NumV y] = if y == 0 then error "division by zero" else NumV (x / y)
primOpApply "<=" [NumV x, NumV y] = BoolV (x <= y)
primOpApply "<" [NumV x, NumV y] = BoolV (x < y)
primOpApply ">=" [NumV x, NumV y] = BoolV (x >= y)
primOpApply ">" [NumV x, NumV y] = BoolV (x > y)
primOpApply "equal?" [x, y] = BoolV (x == y)
primOpApply "and" [BoolV x, BoolV y] = BoolV (x && y)
primOpApply "or" [BoolV x, BoolV y] = BoolV (x || y)
primOpApply "not" [BoolV x] = BoolV (not x)
primOpApply "substring" [StrV s, NumV start, NumV len] =
  let start' = round start
      len' = round len
  in StrV (take len' (drop start' s))
primOpApply "strlen" [StrV s] = NumV (fromIntegral (length s))
primOpApply op args = error $ "unknown primitive operation: " ++ op ++ " with args: " ++ show args