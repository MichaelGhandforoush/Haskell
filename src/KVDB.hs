-- | Key-value database.
module KVDB
  ( KVDB,
    startKVDB,
    kvGet,
    kvPut,
  )
where

import GenServer

-- | A reference to a KVDB instance that stores keys of type 'k' and
-- corresponding values of type 'v'.
type InternalData k v = [(k,v)]

data KVDB k v = KVDB (Server (Msg k v))

data Msg k v = Get k (ReplyChan v) | Put k v

-- | Start a new KVDB instance.
startKVDB :: (Ord k) => IO (KVDB k v)
startKVDB = do
  kvdb <- spawn $ kvdbLoop [] []
  pure (KVDB kvdb)

-- | Retrieve the value corresponding to a given key. If that key does
-- not exist in the store, then this function blocks until another
-- thread writes the desired key with 'kvPut', after which this
-- function returns the now available value.
kvGet :: KVDB k v -> k -> IO v
kvGet kvdb k = case kvdb of
  KVDB db -> requestReply db (Get k)

-- | Write a key-value mapping to the database. Replaces any prior
-- mapping of the key.
kvPut :: KVDB k v -> k -> v -> IO ()
kvPut kvdb k v = case kvdb of
  KVDB db -> sendTo db (Put k v)

kvdbLoop :: Eq k => InternalData k v -> [(k,ReplyChan v)] -> Chan (Msg k v) -> IO ()
kvdbLoop kvdb block input = do
  msg <- receive input
  case msg of
    Get key from-> do
      case lookup key kvdb of
        Just x -> do
          reply from x  
          kvdbLoop kvdb block input
        Nothing -> kvdbLoop kvdb ((key, from):block) input
    Put k v -> 
      case lookup k block of
        Nothing -> do
          let s' = (k, v) : filter ((/= k) . fst) kvdb in kvdbLoop s' block input
        Just blockedChan -> do
          reply blockedChan v
          let s' = (k, v) : filter ((/= k) . fst) kvdb in kvdbLoop s' [] input


