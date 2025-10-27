module APL.InterpConcurrent (runEval) where

import APL.Monad
import Data.IORef
import KVDB
import SPC
import Control.Monad (void)


runEval ::EvalM a -> IO (Either Error a)
runEval input = do 
    ref <- newIORef (Left "Main thread not finished")
    activeSubJobs <- newIORef []
    db <- startKVDB :: IO (KVDB Val Val)
    spc <- startSPC
    void $ runEval' [] db spc activeSubJobs ref input
    readIORef ref
    where
    runEval' :: Env -> (KVDB Val Val) -> SPC -> IORef [JobId] -> IORef (Either Error a) -> EvalM a ->  (IO ())
    runEval' _ _ _ _ ref (Pure x) = writeIORef ref (Right x) --find ud af om atomic eller ikke
    runEval' r kvdb spc j ref (Free (ReadOp k)) = runEval' r kvdb spc j ref $ k r
    runEval' _ _ _ _ ref (Free (ErrorOp e)) = writeIORef ref (Left e) --samme
    runEval' r kvdb spc j ref (Free (StepOp m)) = runEval' r kvdb spc j ref m
    runEval' r kvdb spc j ref (Free (BothOfOp m1 m2 f)) = do
        ref1 <- newIORef (Left "Job1 in BothOfOp not finished")
        ref2 <- newIORef (Left "Job2 in BothOfOp not finished")
        job1 <- jobAdd spc (Job (runEval' r kvdb spc j ref1 m1))
        job2 <- jobAdd spc (Job (runEval' r kvdb spc j ref2 m2))
        atomicModifyIORef j $ \activeJobs -> (job1 : job2 : activeJobs, ())
        _ <- jobWaitAny spc [job1, job2]
        status1 <- jobStatus spc job1 --Måden job fjernes kan forbedres
        _ <- if status1 /= JobDone Done then jobWaitAny spc [job1] --sørg for hvis jobdone crahsed eller timeout and det betragtes som error
        else jobWaitAny spc [job2]
        e1 <- readIORef ref1
        e2 <- readIORef ref2
        case (e1, e2) of
            (Right v1, Right v2) -> runEval' r kvdb spc j ref (f $ ValTuple (v1:[v2]))
            (Left err, _) -> writeIORef ref (Left err)
            (_, Left err) -> writeIORef ref (Left err)    
    runEval' r kvdb spc j ref (Free (OneOfOp m1 m2 f)) = do
        ref1 <- newIORef (Left "Job1 in OneOfOp finished")
        ref2 <- newIORef (Left "Job2 in OneOfOp not finished")
        parentRef <- newIORef False
        job1 <- jobAdd spc (Job (runEval' r kvdb spc j ref1 m1))
        job2 <- jobAdd spc (Job (runEval' r kvdb spc j ref2 m2))
        --parentFinishedJob <- jobAdd spc (Job (isParentJobFinished parentRef))
        let jobs = [job1, job2]
        subJobRef <- newIORef jobs
        --atomicModifyIORef j $ \activeJobs -> (job1 : job2 : activeJobs, ())
        (finishedJob, reason) <- jobWaitAny spc jobs
        --if finishedJob==parentFinishedJob then
        case reason of
            Done -> do
                eFinished <- if finishedJob == job1 then readIORef ref1 else readIORef ref2
                let (otherJob, otherRef) = if finishedJob == job1 then (job2, ref2) else (job1, ref1) in
                    case eFinished of
                        Right x -> finishEval r kvdb spc subJobRef ref (f x)--finishEval r kvdb spc j ref (f x)
                        Left _ -> do 
                            _ <- jobWaitAny spc [otherJob]
                            eOther <- readIORef otherRef
                            case eOther of
                                Right x -> finishEval r kvdb spc subJobRef ref (f x) --finishEval r kvdb spc j ref (f x) 
                                Left err -> finishEval r kvdb spc subJobRef ref (Free (ErrorOp err))--finishEval r kvdb spc j ref (Free (ErrorOp err)) 
            _ -> let (otherJob, otherRef) = if finishedJob == job1 then (job2, ref2) else (job1, ref1) in do
                _ <- jobWaitAny spc [otherJob]
                eOther <- readIORef otherRef
                case eOther of
                    Right x -> finishEval r kvdb spc subJobRef ref (f x)--finishEval r kvdb spc j ref (f x)
                    Left err -> finishEval r kvdb spc subJobRef ref (Free (ErrorOp err))--finishEval r kvdb spc j ref (Free (ErrorOp err))
    runEval' r kvdb spc j ref (Free (KvPutOp k v m)) = do
        kvPut kvdb k v
        --job1 <- jobAdd spc (Job (kvPut kvdb k v))
        --_ <- jobWaitAny spc [job1] --Potentiel fjern unødvendig jobadd
        runEval' r kvdb spc j ref m
    runEval' r kvdb spc j ref (Free (KvGetOp k m)) = do
        v <- kvGet kvdb k
        runEval' r kvdb spc j ref $ m v
    finishEval:: Env -> (KVDB Val Val) -> SPC -> IORef [JobId] -> IORef (Either Error a) -> EvalM a -> IO ()
    finishEval r kvdb spc j ref m = do
        activeJobs <- readIORef j
        mapM_ (jobCancel spc) activeJobs
        runEval' r kvdb spc j ref m

   {-  isParentJobFinished :: IORef Bool -> IO()
    isParentJobFinished r = do
        isFinished <- readIORef r
        if isFinished then pure () else isParentJobFinished r -}
