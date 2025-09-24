module APL.Eval
  (
    Val (..),
    eval,
    Env
  )
where

import APL.AST (Exp (..))
import Test.Tasty.HUnit ((@?=))


type Error = String
type VName = String

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend name val env = env ++ [(name, val)]

envLookup :: VName -> Env -> Maybe Val
envLoopup name [] = Nothing
envLookup name env = case (env !! 0) of
  (name, val) -> Just val
  (_, _) -> envLookup name (drop 1 env)
    



eval :: Env -> Exp -> Either Error Val
--eval :: Exp -> Either Error Val
--eval (Add x y) = Right $ ValInt $ (\(Right (ValInt x)) -> x) (eval x) + (\(Right (ValInt y)) -> y) (eval y)
eval (Add x y) = case (eval x, eval y) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x+y
eval (Sub x y) = case (eval x, eval y) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x-y
eval (Mul x y) = case (eval x, eval y) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x*y  
eval (Eql x y) = case (eval x, eval y) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right( ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
  (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x==y
eval (If x y z) = case eval x of
  Left err -> Left err
  (Right (ValBool True)) -> eval y
  (Right (ValBool False)) -> eval z
  (Right _) -> Left "Non-boolean"

--eval (Sub (CstInt x) (CstInt y)) = ValInt (x-y)
--eval (Mul (CstInt x) (CstInt y)) = ValInt (x*y)
--eval (Div (CstInt x) (CstInt y)) = ValInt (x `div` y)
--eval (Pow (CstInt x) (CstInt y)) = ValInt (x*y)
eval (CstInt x) = Right $ ValInt x
eval (CstBool x) = Right $ ValBool x

