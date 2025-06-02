import Lib
testParseExpr1 = TestCase (assertEqual "parseExpr1" (parse (parseExpr "{if 1 2 3}")) (IfE (NumE 1) (NumE 2) (NumE 3)))
testParseExpr2 = TestCase (assertEqual "parseExpr2" (parse (parseExpr "{with [x = 42] {+ x x}}")) (WithE [("x", NumE 42)] (AppE (LamE ["x"] (NumE 0)) [NumE 42, NumE 42])))
testParseExpr3 = TestCase (assertEqual "parseExpr3" (parse (parseExpr "{x y => {+ x y}}")) (LamE ["x", "y"] (AppE (LamE ["x", "y"] (NumE 0)) [IdE "x", IdE "y"])))

tests = TestList
  [ testParseExpr1
  , testParseExpr2
  , testParseExpr3
  ]

main = runTestTT tests