module APL.InterpConcurrent_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpConcurrent (runEval)
import APL.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

evalTest :: String -> Exp -> Val -> TestTree
evalTest desc e v = testCase desc $ do
  res <- runEval $ eval e
  res @?= Right v

evalTestFail :: String -> Exp -> TestTree
evalTestFail desc e =
  testCase desc $ do
    res <- runEval (eval e)
    case res of
      Left _ -> pure ()
      Right v ->
        assertFailure $
          "Expected error but received this value:\n" ++ show v

tests :: TestTree
tests =
  testGroup
    "Concurrent interpreter"
    {- [evalTest
    "1$$2"
    (BothOf (CstInt 1) (CstInt 2))
    (ValTuple[ValInt 1, ValInt 2]),
    evalTestFail
    "1/0$$1/0"
    (BothOf (Div (CstInt 1) (CstInt 0)) (Div (CstInt 1) (CstInt 0))),
    evalTestFail
    "BothOf erros in each expression that should fail at the same step"
    (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "y") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0))))
    ,
    evalTest
    "1||2"
    (OneOf (CstInt 1) (CstInt 2))
    (ValInt 2),
    evalTest
    "1||2"
    (OneOf (CstInt 10) (CstInt 2))
    (ValInt 2),
    evalTestFail
    "Nested OneOf (OneOf for_err for_err) (OneOf for_err for_err)"
    (OneOf (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 2) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 3) (Div (Var "x") (CstInt 0)))) (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 4) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0)))))
    ,
    evalTest
    "Nested OneOf (OneOf for5 for5) (for9)."
    (OneOf (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))) (ForLoop ("x",CstInt 1) ("i",CstInt 9) (Mul (Var "x") (CstInt 2))))
    (ValInt 512),
    evalTest
    "OneOf (OneOf (inf loop) (fin loop )) (fin loop)"
    (OneOf (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))))
    (ValInt 32),
    evalTest
    "OneOf (OneOf (inf loop) (inf loop )) (fin loop)"
    (OneOf (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))))
    (ValInt 32),
    evalTest
    "OneOf inifinite loop and finite loop"
    (OneOf  (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))(ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))))
    (ValInt 32),
    evalTest
    "finite loop || infinite loop"
    (OneOf  (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))(WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")))
    (ValInt 32),
    evalTestFail
    "OneOf error in second expression"
    (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0))))
    ,
    evalTest
      "get 0 && put 0 true"
      (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstBool True)))
      (ValTuple [ValBool True,ValBool True])
    ] -}
      [evalTest
      "1||2"
      (OneOf (CstInt 1) (CstInt 2))
      (ValInt 2),
      evalTest
      "OneOf inifinite loop and infinite loop"
      (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")))
      (ValInt 32)
      ,
         evalTest
        "put(1,1) && get 1 shared state"
        (BothOf (KvPut (CstInt 1) (CstInt 1)) (KvGet (CstInt 1)))
        (ValTuple [ValInt 1, ValInt 1]),
        evalTest
        "put(1,1) || get 1 shared state"
        (OneOf (Let "x" (KvPut (CstInt 1) (CstInt 1)) (KvGet (CstInt 0))) (KvGet (CstInt 1)))
        (ValInt 1)
        ,

        evalTestFail
        "Project fails when not tuple "
        (Project (CstInt 1) 0)
      {- ,
      evalTest
      "1||2"
      (OneOf (CstInt 10) (CstInt 2))
      (ValInt 2),
      evalTest
      "OneOf inifinite loop and finite loop"
      (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))))
      (ValInt 32),
      evalTest
      "OneOf inifinite loop and infinite loop"
      (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")))
      (ValInt 32),
      evalTest
      "(inf||fin)||(inf||fin)"
      (OneOf (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))) (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))))
      (ValInt 32),
      evalTest
      "Nested OneOf (OneOf for for) (OneOf for for)"
      (OneOf (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 2) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 3) (Mul (Var "x") (CstInt 2)))) (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 9) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 10) (Mul (Var "x") (CstInt 2)))))
      (ValInt 4),
      evalTest
      "Nested OneOf (OneOf for_err for) (OneOf for for)"
      (OneOf (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 2) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 3) (Mul (Var "x") (CstInt 2)))) (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 4) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))))
      (ValInt 8),
      evalTest
      "Nested OneOf (OneOf for_err for_err) (OneOf for for)"
      (OneOf (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 2) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 3) (Div (Var "x") (CstInt 0)))) (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 4) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))))
      (ValInt 16),
      evalTest
      "Nested OneOf (OneOf for for) (OneOf for_err for_err)"
      (OneOf (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 4) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 10000000) (Mul (Var "x") (CstInt 2))))(OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 2) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 3) (Div (Var "x") (CstInt 0)))))
      (ValInt 16),
      evalTest
      "Nested OneOf (OneOf for while_inf) (OneOf for_err for_err)"
      (OneOf (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 4) (Mul (Var "x") (CstInt 2))) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))) (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 2) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 3) (Div (Var "x") (CstInt 0)))))
      (ValInt 16),
      evalTest
      "OneOf (for) (OneOf while_inf while_inf)"
      (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 4) (Mul (Var "x") (CstInt 2)))  (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))))
      (ValInt 16),
      evalTestFail
      "Nested OneOf (OneOf for_err for_err) (OneOf for_err for_err)"
      (OneOf (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 2) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 3) (Div (Var "x") (CstInt 0)))) (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 4) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0)))))
      ,
      evalTest
      "Nested OneOf (OneOf for5 for5) (for9)."
      (OneOf (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))) (ForLoop ("x",CstInt 1) ("i",CstInt 9) (Mul (Var "x") (CstInt 2))))
      (ValInt 512),
      evalTest
      "OneOf (OneOf (inf loop) (fin loop )) (fin loop)"
      (OneOf (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))))
      (ValInt 32),
      evalTest
      "OneOf (OneOf (inf loop) (inf loop )) (fin loop)"
      (OneOf (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))))
      (ValInt 32),
      evalTest
      "OneOf inifinite loop and finite loop"
      (OneOf  (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))(WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")))
      (ValInt 32),
      evalTestFail
      "OneOf error in second expression"
      (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0)))),
      evalTest
      "BothOf 2 equal finite loop"
      (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))))
      (ValTuple [ValInt 32, ValInt 32]),
      evalTest
      "BothOf 2 finite loop ending at different times"
      (BothOf (ForLoop ("x",CstInt 2) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))))
      (ValTuple [ValInt 64, ValInt 32]),
      evalTestFail
      "BothOf error in second expression"
      (BothOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0)))),
      evalTestFail
      "BothOf erros in each expression that should fail at the same step"
      (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "y") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0)))),
      evalTest
      "Nested BothOf BothOf(for for) BothOf(for for)"
      (BothOf (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))) (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))))
      (ValTuple [ValTuple [ValInt 32, ValInt 32], ValTuple [ValInt 32, ValInt 32]]),
      evalTestFail
      "Nested BothOf BothOf(for_err for) BothOf(for for)"
      (BothOf (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))) (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))))),
      evalTestFail
      "Nested BothOf BothOf(for_err for_err) BothOf(for for)"
      (BothOf (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0)))) (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))))),
      evalTestFail
      "Nested BothOf BothOf(for_err for_err) BothOf(for_err for_err)"
      (BothOf (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0)))) (BothOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0))) (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Div (Var "x") (CstInt 0)))))
      ,
      evalTest
      "Test threads in background nested and"
      (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))) (BothOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))))
      (ValInt 32)
      ,evalTest
      "Test threads in background nestd or"
      (OneOf (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2))) (OneOf (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"))))
      (ValInt 32)
      ,evalTest
      "get 0 && put 0 true"
      (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstBool True)))
      (ValTuple [ValBool True,ValBool True]),
      evalTest
      "get 0 + 1 && put 0 2"
      (BothOf (Add (KvGet (CstInt 0)) (CstInt 1)) (KvPut (CstInt 0) (CstInt 2)))
      (ValTuple [ValInt 3,ValInt 2]),
      evalTest
      "put (get 0) 1 && put 0 2"
      (BothOf (KvPut (KvGet (CstInt 0)) (CstInt 1)) (KvPut (CstInt 0) (CstInt 2)))
      (ValTuple [ValInt 1,ValInt 2]),
      evalTest
      "put (get 0) 1 && let x = put 0 2 in get 2"
      (BothOf (KvPut (KvGet (CstInt 0)) (CstInt 1)) (Let "x" (KvPut (CstInt 0) (CstInt 2)) (KvGet (CstInt 2))))
      (ValTuple [ValInt 1,ValInt 1]),
      evalTest
      "put (0)"
      (KvPut (CstInt 0) (CstInt 1))
      (ValInt 1),
      evalTestFail
      "get (0)"
      (KvGet (CstInt 0)),
      evalTest
      "put (0) get(0)"
      (Let "x" (KvPut (CstInt 0) (CstInt 1)) (KvGet (CstInt 0)))
      (ValInt 1),

      evalTest
      "(e1,e2) - Correct tuple eval order"
      (Tuple [CstInt 1, CstInt 2])
      (ValTuple [ValInt 1, ValInt 2]),
      evalTestFail
      "Test deadlock"
      (BothOf (Let "x" (KvGet (CstInt 2)) (KvPut (CstInt 1) (CstInt 10))) (Let "y" (KvGet (CstInt 1)) (KvPut (CstInt 2)(CstInt 9))))
       -}
      ]
