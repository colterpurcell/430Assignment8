import Lib
import Test.HUnit

testInterp_addition = TestLabel "testInterp_addition" (TestCase (interp (AppE (IdE "+") [NumE 2, NumE 3]) primEnv @?= NumV 5))
testInterp_subtraction = TestLabel "testInterp_subtraction" (TestCase (interp (AppE (IdE "-") [NumE 5, NumE 3]) primEnv @?= NumV 2))
testInterp_multiplication = TestLabel "testInterp_multiplication" (TestCase (interp (AppE (IdE "*") [NumE 4, NumE 5]) primEnv @?= NumV 20))
testInterp_division = TestLabel "testInterp_division" (TestCase (interp (AppE (IdE "/") [NumE 10, NumE 2]) primEnv @?= NumV 5))
testInterp_boolean = TestLabel "testInterp_boolean" (TestCase (interp (BoolE True) primEnv @?= BoolV True))
testInterp_if_true = TestLabel "testInterp_if_true" (TestCase (interp (IfE (BoolE True) (NumE 1) (NumE 0)) primEnv @?= NumV 1))
testInterp_if_false = TestLabel "testInterp_if_false" (TestCase (interp (IfE (BoolE False) (NumE 1) (NumE 0)) primEnv @?= NumV 0))
testInterp_with_bindings = TestLabel "testInterp_with_bindings" (TestCase (interp (WithE [("x", NumE 10)] (AppE (IdE "+") [IdE "x", NumE 5])) primEnv @?= NumV 15))
testInterp_lambda_application = TestLabel "testInterp_lambda_application" (TestCase (interp (AppE (LamE ["x"] (AppE (IdE "+") [IdE "x", NumE 1])) [NumE 4]) primEnv @?= NumV 5))

main :: IO ()
main = runTestTTAndExit $ TestList
  [ testInterp_addition
  , testInterp_subtraction
  , testInterp_multiplication
  , testInterp_division
  , testInterp_boolean
  , testInterp_if_true
  , testInterp_if_false
  , testInterp_with_bindings
  , testInterp_lambda_application
  ]