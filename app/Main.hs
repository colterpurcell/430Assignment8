import Lib

{-
OUR GUIDING LIGHT 👼 (defines what is a valid expression
‹expr› ::= ‹num›
  | ‹id› ; identifier (variable or function name) but not => with or =
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

