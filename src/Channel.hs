module Channel 

where


import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent

--type Stream a = 

data Item a = MkItem a (MVar (Item a))

item1 :: IO (Item Int)
item1 = do 
    emptyMVar <- newEmptyMVar
    return $ MkItem 14 emptyMVar

item0 :: IO (Item Int)
item0 = do     
    it <- item1
    newMVar <- newEmptyMVar
    putMVar newMVar it
    return $ MkItem 2 newMVar 



