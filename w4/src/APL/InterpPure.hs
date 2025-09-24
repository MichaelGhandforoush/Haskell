module APL.InterpPure (runEval) where

import APL.Monad
    ( EvalM,
      EvalOp(StatePutOp, ReadOp, StateGetOp),
      Free(Free, Pure),
      Env,
      State,
      stateInitial,
      envEmpty )

runEval :: EvalM a -> a
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> a
    runEval' _  _ (Pure x) = x
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp k a)) = runEval' r k a
    
