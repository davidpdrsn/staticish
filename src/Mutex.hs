module Mutex where

import Control.Concurrent

type Mutex = MVar ()

claimMutex :: Mutex -> IO ()
claimMutex = takeMVar

newMutex :: IO Mutex
newMutex = newMVar ()

releaseMutex :: Mutex -> IO ()
releaseMutex m = putMVar m ()

withMutex :: Mutex -> IO a -> IO a
withMutex mutex x = do
    claimMutex mutex
    result <- x
    releaseMutex mutex
    return result

