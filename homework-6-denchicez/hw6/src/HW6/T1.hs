{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (STM, MonadConc)
import Control.Monad (when)
import Control.Monad.Conc.Class (atomically)
import Control.Monad.STM.Class (MonadSTM)
import Control.Concurrent.Classy.STM (TArray, TVar, newTVar, readTVar, writeTVar, readTVarConc)
import Data.Array.MArray
import Data.Hashable

-- | Init capacity of concurrent hash table
initCapacity :: Int
initCapacity = 16

-- | Constant to refactor concurrent hash table
loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

justPutCHT :: (MonadSTM m, Hashable a, Eq a) => CHT m a b -> (a, b) -> m ()
justPutCHT table (key, value) = do
  bucketsArr <- readTVar (chtBuckets table)
  (_, capacity) <- getBounds bucketsArr
  let indexKey = hash key `mod` (capacity + 1)
  bucket <- readArray bucketsArr indexKey
  bucketSz <- readTVar (chtSize table)
  let (deleteBucket, flag) = deleteByKey key bucket
  let newBucket = (key, value) : deleteBucket
  let newBucketSz = if flag then bucketSz else bucketSz + 1
  writeArray bucketsArr indexKey newBucket
  writeTVar (chtBuckets table) bucketsArr
  writeTVar (chtSize table) newBucketSz

doRefactorCHM :: (MonadSTM m, Eq k, Hashable k) => CHT m k v -> m ()
doRefactorCHM table = do
  bucketsArrWas <- readTVar (chtBuckets table)
  (_, capacity) <- getBounds bucketsArrWas
  buckets <- newArray (0, capacity * 2) []
  writeTVar (chtBuckets table) buckets
  writeTVar (chtSize table) 0
  getBuckets <- getElems bucketsArrWas
  mapM_ (mapM (justPutCHT table)) getBuckets

deleteByKey :: Eq k => k -> [(k, v)] -> ([(k, v)], Bool)
deleteByKey keyFind ((key, x): xs) = if keyFind == key
                                      then (xs, True)
                                      else ((key, x): arr, flag) where (arr, flag) = deleteByKey keyFind xs
deleteByKey _ [] = ([], False)

-- | Create new concurrent hash table
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  buckets <- newArray (0, initCapacity - 1) []
  chtBucketsEmpty <- newTVar buckets
  chtZero <- newTVar 0
  pure (CHT {chtBuckets=chtBucketsEmpty, chtSize=chtZero})

-- | Get value by key in concurrent hash table
getCHT :: (MonadConc m, Eq k, Hashable k) => k -> CHT (STM m) k v -> m (Maybe v)
getCHT key table = atomically $ do
  bucketsArr <- readTVar (chtBuckets table)
  (_, capacity) <- getBounds bucketsArr
  let indexKey = hash key `mod` (capacity + 1)
  bucket <- readArray bucketsArr indexKey
  pure (lookup key bucket)

-- | Get value by key in concurrent hash table
putCHT :: (MonadConc m, Eq k, Hashable k) => k -> v -> CHT (STM m) k v -> m ()
putCHT key value table = atomically $ do
  justPutCHT table (key, value)
  bucketsArr <- readTVar (chtBuckets table)
  (_, capacity) <- getBounds bucketsArr
  bucketSz <- readTVar (chtSize table)
  when (fromIntegral bucketSz >= fromIntegral capacity * loadFactor) (doRefactorCHM table)

-- | Get size of concurrent hash table
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT table = readTVarConc (chtSize table)