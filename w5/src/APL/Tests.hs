module APL.Tests where

import APL.AST (Exp (..), VName)
import Test.QuickCheck (Gen, elements, arbitrary, oneof, sample, sized, Arbitrary)
import APL.Eval (eval, runEval)
import Test.QuickCheck.Arbitrary (shrink)
genVar :: Gen VName
genVar = abs <$> (arbitrary :: Gen Int) >>= go where
    go 0 = pure ""
    go n = (++) <$> elements ["a","b","c","d"] <*> go2 (n-1)
    go2 0 = pure ""
    go2 n = (++) <$> elements ["a", "b", "c", "1", "2", "3"] <*> go2 (n-1)


genExp :: Int -> Gen Exp
genExp n = 
    if n <= 1 then Var <$> genVar
    else 
        oneof [Var <$> genVar, Lambda <$> genVar <*> genExp (n-2), Apply <$> genExp (n-3) <*> genExp (n-3), Add <$> genExp (n-3) <*> genExp (n-3)]

prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc x y z= (z+y)+z == x+(y+z)

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc x y z = runEval (eval (Add (Add x y) z)) == runEval (eval (Add x (Add y z)))

instance Arbitrary Exp where
    arbitrary = sized genExp
    shrink (Add e1 e2) = [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2] ++ [e1] ++ [e2]
    shrink (Var n) = if n=="" then [Var n' | n' <- n] 
