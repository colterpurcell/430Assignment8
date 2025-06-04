import Lib
import Test.HUnit
import Control.Exception (try, evaluate, SomeException)

testInterp_addition, testInterp_subtraction, testInterp_multiplication, testInterp_division :: Test
testInterp_boolean, testInterp_if_true, testInterp_if_false, testInterp_with_bindings :: Test
testInterp_lambda_application, testserialize :: Test
testInterp_addition = TestLabel "testInterp_addition" (TestCase (interp (AppE (IdE "+") [NumE 2, NumE 3]) primEnv @?= NumV 5))
testInterp_subtraction = TestLabel "testInterp_subtraction" (TestCase (interp (AppE (IdE "-") [NumE 5, NumE 3]) primEnv @?= NumV 2))
testInterp_multiplication = TestLabel "testInterp_multiplication" (TestCase (interp (AppE (IdE "*") [NumE 4, NumE 5]) primEnv @?= NumV 20))
testInterp_division = TestLabel "testInterp_division" (TestCase (interp (AppE (IdE "/") [NumE 10, NumE 2]) primEnv @?= NumV 5))
testInterp_boolean = TestLabel "testInterp_boolean" (TestCase (interp (BoolE True) primEnv @?= BoolV True))
testInterp_if_true = TestLabel "testInterp_if_true" (TestCase (interp (IfE (BoolE True) (NumE 1) (NumE 0)) primEnv @?= NumV 1))
testInterp_if_false = TestLabel "testInterp_if_false" (TestCase (interp (IfE (BoolE False) (NumE 1) (NumE 0)) primEnv @?= NumV 0))
testInterp_with_bindings = TestLabel "testInterp_with_bindings" (TestCase (interp (WithE [("x", NumE 10)] (AppE (IdE "+") [IdE "x", NumE 5])) primEnv @?= NumV 15))
testInterp_lambda_application = TestLabel "testInterp_lambda_application" (TestCase (interp (AppE (LamE ["x"] (AppE (IdE "+") [IdE "x", NumE 1])) [NumE 4]) primEnv @?= NumV 5))
testserialize = TestLabel "testSerialize" (TestList
  [ "serialize number"     ~: serialize (NumV 34)      ~?= "34.0"
  , "serialize true"       ~: serialize (BoolV True)   ~?= "true"
  , "serialize false"      ~: serialize (BoolV False)  ~?= "false"
  , "serialize string"     ~: serialize (StrV "hello") ~?= "\"hello\""
  , "serialize closure"    ~: serialize (CloV ["x"] (NumE 1) []) ~?= "#<procedure>"
  , "serialize primop"     ~: serialize (PrimV "+")    ~?= "#<primop>"
  ])

testPrimOpApply_addition, testPrimOpApply_subtraction, testPrimOpApply_multiplication, testPrimOpApply_division :: Test
testPrimOpApply_divisionByZero, testPrimOpApply_lessThanOrEqual, testPrimOpApply_lessThan :: Test
testPrimOpApply_greaterThanOrEqual, testPrimOpApply_greaterThan, testPrimOpApply_equal :: Test
testPrimOpApply_and, testPrimOpApply_or, testPrimOpApply_not :: Test

testPrimOpApply_addition = TestLabel "testPrimOpApply_addition" (TestCase (primOpApply "+" [NumV 2, NumV 3] @?= NumV 5))
testPrimOpApply_subtraction = TestLabel "testPrimOpApply_subtraction" (TestCase (primOpApply "-" [NumV 5, NumV 3] @?= NumV 2))
testPrimOpApply_multiplication = TestLabel "testPrimOpApply_multiplication" (TestCase (primOpApply "*" [NumV 4, NumV 5] @?= NumV 20))
testPrimOpApply_division = TestLabel "testPrimOpApply_division" (TestCase (primOpApply "/" [NumV 10, NumV 2] @?= NumV 5))
testPrimOpApply_divisionByZero = TestLabel "testPrimOpApply_divisionByZero" (
    TestCase (
      do
        result <- try (evaluate (primOpApply "/" [NumV 10, NumV 0])) :: IO (Either SomeException Value)
        case result of
          Left _  -> return ()  -- this means an exception was thrown, which is expected
          Right _ -> assertFailure "Expected division by zero error, but got a result"
    ))
testPrimOpApply_lessThanOrEqual = TestLabel "testPrimOpApply_lessThanOrEqual" (TestCase (primOpApply "<=" [NumV 2, NumV 3] @?= BoolV True))
testPrimOpApply_lessThan = TestLabel "testPrimOpApply_lessThan" (TestCase (primOpApply "<" [NumV 2, NumV 3] @?= BoolV True))
testPrimOpApply_greaterThanOrEqual = TestLabel "testPrimOpApply_greaterThanOrEqual" (TestCase (primOpApply ">=" [NumV 3, NumV 2] @?= BoolV True))
testPrimOpApply_greaterThan = TestLabel "testPrimOpApply_greaterThan" (TestCase (primOpApply ">" [NumV 3, NumV 2] @?= BoolV True))
testPrimOpApply_equal = TestLabel "testPrimOpApply_equal" (TestCase (primOpApply "equal?" [NumV 2, NumV 2] @?= BoolV True))
testPrimOpApply_and = TestLabel "testPrimOpApply_and" (TestCase (primOpApply "and" [BoolV True, BoolV False] @?= BoolV False))
testPrimOpApply_or = TestLabel "testPrimOpApply_or" (TestCase (primOpApply "or" [BoolV True, BoolV False] @?= BoolV True))
testPrimOpApply_not = TestLabel "testPrimOpApply_not" (TestCase (primOpApply "not" [BoolV True] @?= BoolV False))

testSubstring, testPrimOpApply_unknownOp :: Test
testSubstring = TestLabel "testSubstring" (TestCase (primOpApply "substring" [StrV "hello", NumV 1, NumV 3] @?= StrV "ell"))

testPrimOpApply_unknownOp =
  TestLabel "testPrimOpApply_unknownOp" (
    TestCase (
      do
        result <- try (evaluate (primOpApply "::" [NumV 1])) :: IO (Either SomeException Value)
        case result of
          Left _  -> return () 
          Right _ -> assertFailure "Expected unknown primitive operation error, but got a result"
    )
  )

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
  , testserialize
  , testPrimOpApply_addition
  , testPrimOpApply_subtraction
  , testPrimOpApply_multiplication
  , testPrimOpApply_division
  , testPrimOpApply_divisionByZero
  , testPrimOpApply_lessThanOrEqual
  , testPrimOpApply_lessThan
  , testPrimOpApply_greaterThanOrEqual
  , testPrimOpApply_greaterThan 
  , testPrimOpApply_equal
  , testPrimOpApply_and
  , testPrimOpApply_or
  , testPrimOpApply_not
  , testSubstring
  , testPrimOpApply_unknownOp
  ]