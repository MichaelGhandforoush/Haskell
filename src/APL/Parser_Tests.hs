module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ -- Example tests
      parserTest "x+y" $ Add (Var "x") (Var "y"),
      parserTestFail "x+",
      parserTest "()" $ Tuple [],
      parserTest "(x, y)" $ Tuple [Var "x", Var "y"],
      parserTest "(x , y)" $ Tuple [Var "x", Var "y"],
      parserTest "(x/y, y)" $ Tuple [Div (Var "x") (Var "y"), Var "y"],
      parserTest "(x , y)" $ Tuple [Var "x", Var "y"],
      parserTest "(x)" $ Var "x",
      parserTest "(x, y, z)" $ Tuple [Var "x", Var "y", Var "z"],
      parserTest "((x, y), z)" $ Tuple [Tuple [Var "x", Var "y"], Var "z"],
      parserTest "x " $ Var "x",
      parserTest "x.0" $ Project (Var "x") 0,
      parserTest "let x = (1,2) in x.0" $ Let "x" (Tuple [CstInt 1,CstInt 2]) (Project (Var "x") 0),
      parserTest "(1,2).1" $ Project (Tuple [CstInt 1,CstInt 2]) 1,
      parserTest "loop x = 1 for i < 5 do x * 2" $ ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)),
      parserTest "loop x = 1 while x == 1 do x" $ WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"),
      parserTest "(1+2) && (3+4)" $ BothOf (Add (CstInt 1) (CstInt 2)) (Add (CstInt 3) (CstInt 4)),
      parserTest "1+2 && 3+4" $ BothOf (Add (CstInt 1) (CstInt 2)) (Add (CstInt 3) (CstInt 4)),
      parserTest "(1+2) || (3+4+5+6)" $ OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6))

      ,parserTest "(x+y).0" $ Project (Add (Var "x") (Var "y")) 0
      ,parserTest "(x+y).1" $ Project (Add (Var "x") (Var "y")) 1
      ,parserTest "(x,y).2" $ Project (Tuple [Var "x",Var "y"]) 2
      ,parserTest "(if x then y else z, a)" $ Tuple [If (Var "x") (Var "y") (Var "z"),Var "a"]
      ,parserTest "(a b)" $ Apply (Var "a") (Var "b")
      ,parserTest "(())" $ Tuple []
      ,parserTest "((),())" $ Tuple[Tuple [], Tuple []]
      ,parserTest "(x,y,z)+(a,b,c,d)" $ Add (Tuple [Var "x", Var "y", Var "z"]) (Tuple [Var "a", Var "b", Var "c", Var "d"])
      
      ,parserTestFail "x|" 
      ,parserTestFail "x|y"
      ,parserTestFail "x&"
      ,parserTestFail "x||"
      ,parserTestFail "x&&"
      ,parserTest "x||y" $ OneOf (Var "x") (Var "y")
      ,parserTest "x&&y" $ BothOf (Var "x") (Var "y")
      ,parserTest "x||y&&x&&y" $ OneOf (Var "x") (BothOf (BothOf (Var "y") (Var "x")) (Var "y"))
      ,parserTest "x||y+z" $ OneOf (Var "x") (Add (Var "y") (Var "z"))
      ,parserTest "loop x = 1 while x == 1 do x||y" $ WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (OneOf (Var "x") (Var "y"))

    ]
