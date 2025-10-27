module APL.InterpPure (runEval) where

import APL.Monad
    ( EvalM,
      EvalOp(ErrorOp, ReadOp, KvGetOp, KvPutOp, StepOp, BothOfOp, OneOfOp),
      Free(Free, Pure),
      Env,
      Error,
      Val (ValTuple),
      envEmpty )

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

runEval :: EvalM a -> Either Error a
runEval = fmap fst $ runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> (Either Error a, State)
    runEval' _ s (Pure x) = (pure x, s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> (Left $ "Invalid key: " ++ show key, s)
        Just val -> runEval' r s $ k val
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s
       in runEval' r s' m
    runEval' r s (Free (StepOp e)) = runEval' r s e
    runEval' r s (Free (BothOfOp m1 m2 f)) = 
      let (v1,s1) = runEval' r s m1 
          (v2,s2) = runEval' r s1 m2
      in
        case (v1, v2) of
          (Right val1, Right val2) -> runEval' r s2 (f $ ValTuple (val1:[val2]))
          (Left err, _) -> (Left err, s1)
          (_, Left err) -> (Left err, s2)
    runEval' r s (Free (OneOfOp m1 m2 f)) = 
      let (v1, s1) = runEval' r s m1 in
        case v1 of
          Right val1 -> runEval' r s1 (f val1)
          _ ->
            let (v2, s2) = runEval' r s1 m2 in
              case v2 of
                Right val2 -> runEval' r s2 (f val2)
                Left err -> (Left err, s2)
    runEval' _ s (Free (ErrorOp e)) = (Left e, s)
    
