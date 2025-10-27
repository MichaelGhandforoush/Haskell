module APL.InterpPure_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpPure (runEval)
import APL.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

evalTest :: String -> Exp -> Val -> TestTree
evalTest desc e v =
  testCase desc $
    runEval (eval e) @?= Right v

evalTestFail :: String -> Exp -> TestTree
evalTestFail desc e =
  testCase desc $
    case runEval (eval e) of
      Left _ -> pure ()
      Right v ->
        assertFailure $
          "Expected error but received this value:\n" ++ show v

tests :: TestTree
tests =
  testGroup
    "Pure interpreter"
    [ evalTestFail
        "State (unknown key)"
        (KvGet (CstInt 0)),
      --
      -- Should work after task A.
      evalTest
        "(e1,e2)"
        (Tuple [CstInt 1, CstInt 2])
        (ValTuple [ValInt 1, ValInt 2]),
      evalTestFail
        "(1/0,e2)"
        (Tuple [Div (CstInt 1) (CstInt 0), CstInt 2])
        ,

      evalTest
      "let x = (1,2) in x.0"
      (Let "x" (Tuple [CstInt 1,CstInt 2]) (Project (Var "x") 0))
      (ValInt 1),
      evalTest
      "(1,2).1"
      (Project (Tuple [CstInt 1,CstInt 2]) 1)
      (ValInt 2),
      evalTest
      "() #empty tuple"
      (Tuple [])
      (ValTuple []),
      evalTestFail
      "().0"
      (Project (Tuple []) 0),
      evalTestFail
      "(1,2,3).4"
      (Project (Tuple []) 0),
      evalTestFail
      "(1,2).2"
      (Project (Tuple [CstInt 1,CstInt 2]) 2),

      evalTest
        "For loop using bindings"
        (ForLoop ("x", CstInt 1) ("i", CstInt 3) (Add (Var "i") (Var "x")))
        (ValInt 4),
      evalTestFail
        "For loop using p in bound"
        (ForLoop ("x", CstInt 1) ("i", Var "x") (Add (Var "i") (Var "x")))
        ,
      

      --
      -- Should work after Task B.
      evalTest
        "For loop"
        (ForLoop ("x", CstInt 1) ("i", CstInt 10) (Mul (Var "x") (CstInt 2)))
        (ValInt 1024),


      evalTest
      "loop x = 1 for i < 5 do x * 2"
      (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))
      (ValInt 32),

      evalTest
      "loop x = (1,10) while if (x.1 == 0) then false else true do (x.0*2,x.1-1)"
      (WhileLoop ("x",Tuple [CstInt 1,CstInt 10]) (If (Eql (Project (Var "x") 1) (CstInt 0)) (CstBool False) (CstBool True)) (Tuple [Mul (Project (Var "x") 0) (CstInt 2),Sub (Project (Var "x") 1) (CstInt 1)]))
      (ValTuple [ValInt 1024,ValInt 0]),

      evalTestFail
      "loop x = 1 while x == 1 do x (infinite loop)"
      (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")),

      evalTestFail
      "For loop invalid bound"
      (ForLoop ("x", CstInt 1) ("i", CstBool True) (Mul (Var "x") (CstInt 2))),

      --
      -- Should work after task C.
      evalTest
        "e1 && e2"
        (BothOf (CstInt 0) (CstInt 1))
        (ValTuple [ValInt 0, ValInt 1]),
      --
      -- Should work after task C.
      evalTest
        "e1 || e2"
        (OneOf (CstInt 0) (CstInt 1))
        (ValInt 0), 
      --
      -- Should work after task C.
      evalTest
        "e1 || e2 (first fails)"
        (OneOf (KvGet (CstInt 0)) (CstInt 1))
        (ValInt 1),
      evalTest
        "e1 || e2 (second fails)"
        (OneOf (KvGet (CstInt 0)) (CstInt 1))
        (ValInt 1),
      evalTestFail
        "e1 && e2 (first fails)"
        (BothOf (KvGet (CstInt 0)) (CstInt 1))
        ,
      evalTestFail
        "e1 && e2 (second fails)"
        (BothOf (CstInt 1) (KvGet (CstInt 1)))
        ,

      evalTest
        "e1 || e2 (first fails with div)"
        (OneOf (Div (CstInt 1) (CstInt 0)) (CstInt 1))
        (ValInt 1),
      evalTestFail
        "e1 || e2 (both fail)"
        (OneOf (KvGet (CstInt 0)) (KvGet (CstInt 1))),
      evalTestFail
        "e1 || e2 (both fail with div)"
        (OneOf (Div (CstInt 1) (CstInt 0)) (Div (CstInt 1) (CstInt 0))),
      evalTestFail
        "e1 $$ e2 (both fail with div)"
        (BothOf (Div (CstInt 1 )(CstInt 0)) (Div (CstInt 1 )(CstInt 0))),
        evalTestFail
        "e1 || e2 (both fail)"
        (OneOf (KvGet (CstInt 0)) (KvGet (CstInt 1)))
        ,
      evalTestFail
        "kvget invalid"
        (KvGet (CstInt 0))

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
    ]
