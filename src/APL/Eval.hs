module APL.Eval
  ( eval,
  )
where

import APL.AST (Exp (..))
import APL.Monad
    ( EvalM,
      Val(..),
      envExtend,
      envLookup,
      askEnv,
      localEnv,
      failure,
      evalStep,
      evalBothOf,
      evalOneOf, evalKvGet, evalKvPut)

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "If: non-boolean conditional"
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      evalStep $ localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (Tuple es) = do
  vs <- evalElement es
  pure $ ValTuple vs
  where
    evalElement (x:xs) = do
      v1 <- eval x
      vRest <- evalElement xs
      pure $ v1 : vRest
    evalElement [] = pure []
eval (Project es i) = do
  evalEs <- eval es
  case evalEs of
    ValTuple tuple -> if i < fromIntegral (length tuple) then pure $ tuple !! (fromInteger i) else failure "Index out of range"
    _ -> failure "Projection of non-tuple"
eval (ForLoop (p, init1) (i, bound) body) = do
  v0 <- eval init1
  n <- eval bound
  case n of
    ValInt x -> forloop v0 0 x
    _ -> failure "Bound expression does not evaluate to an integer"
  where
    --forloop :: VName -> Val -> Val -> Val -> Exp -> EvalM Val 
    forloop v iVal n = if iVal < n
      then do
        v' <- evalStep $ localEnv (envExtend p v . envExtend i (ValInt iVal)) $ eval body
        forloop v' (iVal+1) n
      else pure v
eval (WhileLoop (p, init1) cond body) = do
  v0 <- eval init1
  whileloop v0
  where
    whileloop v = do
      c <- localEnv (envExtend p v) $ eval cond
      case c of
        ValBool True -> do 
          v' <- evalStep $ localEnv (envExtend p v) $ eval body
          whileloop v'
        ValBool False -> pure v
        _ -> failure "Condition does not evaluate to a boolean"
eval (BothOf e1 e2) = do
  evalBothOf (eval e1) (eval e2)
eval (OneOf e1 e2) = do
  evalOneOf (eval e1) (eval e2)
eval (KvGet e) = do
  v1 <- eval e
  evalKvGet v1
eval (KvPut key v) = do
  a <- eval key
  b <- eval v
  evalKvPut a b
  pure b
  
