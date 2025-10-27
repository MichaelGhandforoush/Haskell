module APL.InterpSim (runEval) where

import APL.Monad ( EvalM, Free(Pure), Env, Error, Val (ValTuple), EvalOp(ErrorOp, ReadOp, KvGetOp, KvPutOp, StepOp, BothOfOp, OneOfOp), Free(Free, Pure))

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

-- | Continue execution of the provided computation as far as
-- possible, but executing at most one 'StepOp' effect. Any nested
-- computations (in 'BothOp' and 'OneOfOp') must also be stepped
-- similarly. If the computation is stuck on a 'KvGetOp' for which the
-- key is not in the state, then the computation is merely returned
-- unchanged.
--
-- Evaluation of 'BothOp':
--
-- * If either of the nested computations are 'Free (ErrorOp ...)',
--   then propagate that error.
--
-- * If both are 'Pure', then return a pair of the results.
--
-- * Otherwise evaluate both one step.
--
-- Evaluation of 'OneOfOp':
--
-- * If both of the nested computations are 'Free (ErrorOp ...)', then
--   propagate one of the errors.
--
-- * If one is 'Pure', then return that result.
--
-- * Otherwise evaluate both one step.
step :: Env -> State -> EvalM a -> (EvalM a, State)
step _ s (Pure x) = (pure x, s)
step _ s (Free (StepOp m)) = (m, s)
step r s (Free (ReadOp k)) = step r s $ k r
step r s (Free (KvGetOp key k)) =
    case lookup key s of
        Nothing -> (Free (KvGetOp key k), s)
        Just val -> step r s $ k val
step r s (Free (KvPutOp key val m)) =
    let s' = (key, val) : filter ((/= key) . fst) s
    in step r s' m
step r s (Free (OneOfOp m1 m2 f)) = 
    let (e1, s1) = step r s m1 in
        case (e1, m2) of
            (Free(ErrorOp err1), Free(ErrorOp _)) -> step r s1 (Free(ErrorOp err1))
            (Pure v, _) -> step r s1 (f v)
            _ -> (Free (OneOfOp m2 e1 f), s1)
step r s (Free (BothOfOp m1 m2 f)) = 
    let (e1, s1) = step r s m1 in
        case (e1, m2) of
            (Free(ErrorOp e), _) -> step r s1 (Free (ErrorOp e))
            (Pure v1, Pure v2) -> step r s1 (f $ ValTuple (v1:[v2]))
            (Pure _, _) -> (Free (BothOfOp m2 e1 f), s1)
            (_, Pure _) -> (Free (BothOfOp e1 m2 f), s1)
            _ -> (Free (BothOfOp m2 e1 f), s1)    
step _ s (Free (ErrorOp e)) = (Free (ErrorOp e), s)


runEval :: EvalM a -> Either Error a
runEval = fmap fst $ runEval' [] stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> (Either Error a, State)
    runEval' _ s (Pure x) = (Right x, s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (OneOfOp m1 m2 f)) =
        let (e1, s1) = step r s m1 
            (e2, s2) = step r s m2 in 
        case (e1, e2) of
            (Free (ErrorOp err1), Free (ErrorOp _)) -> (Left err1, s1)
            (Pure v, _) -> runEval' r (s1++s2) (f v)
            (_, Pure v) -> runEval' r (s2++s2) (f v)
            (c1, c2) -> runEval' r (s1++s2) (Free (OneOfOp c1 c2 f))
    runEval' r s (Free (BothOfOp m1 m2 f)) = 
        let (e1, s1) = step r s m1 
            (e2, s2) = step r s m2 in 
        case (e1, e2) of
            (Free (ErrorOp err), _) -> (Left err, s1)
            (_, Free (ErrorOp err)) -> (Left err, s2)
            (Pure v1, Pure v2) -> runEval' r s2 (f $ ValTuple (v1:[v2]))
            (Pure v1, c2) -> runEval' r (s1++s2) (Free (BothOfOp (pure v1) c2 f))
            (c1, Pure v2) -> runEval' r (s2++s2) (Free (BothOfOp c1 (pure v2) f))
            (c1, c2) -> runEval' r (s1++s2) (Free (BothOfOp c1 c2 f))
    runEval' r s (Free (KvGetOp key k)) = --Sørg for der ikke kan være noget concurrency, tror godt der kan. 
      case lookup key s of
        Nothing -> (Left $ "Invalid key: " ++ show key ++ ". Concurrency evaluation possible to add the invalid key.", s)
        Just val -> runEval' r s $ k val
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s
       in runEval' r s' m
    runEval' _ s (Free (ErrorOp err)) = (Left err, s)
    runEval' r s (Free (StepOp m)) = let (e1, _) = step r s m in runEval' r s e1



