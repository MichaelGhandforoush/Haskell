module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

evalTest :: TestTree
evalTest = testCase "Exp to Val" $ (eval (CstInt 1)) @?= (Right $ ValInt 1)
addTest :: TestTree
addTest = testCase "Add" $ (eval (Add (CstInt 1) (CstInt 1))) @?= (Right $ ValInt 2)



tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [evalTest,
    addTest]