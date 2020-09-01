module Channel (
    runChannel
)

where


import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent

--type Stream a = 

data Item a = MkItem a (MVar (Item a))

type Channel a = (MVar (MVar (Item a)), MVar (MVar (Item a)))

newChan' :: IO (Channel a)
newChan' = do 
    read <- newEmptyMVar 
    write <- newEmptyMVar
    hole <- newEmptyMVar
    putMVar read hole
    putMVar write hole
    return (read, write)

putChan :: Channel a -> a -> IO ()
putChan (read,write) val = do
    oldHole <- takeMVar write
    newHole <- newEmptyMVar
    putMVar oldHole (MkItem val newHole)
    putMVar write newHole

getChan :: Channel a -> IO a
getChan (read,write) = do    
    head <- takeMVar read
    MkItem val nextHead <- takeMVar head
    putMVar read nextHead
    return val

runChannel = do 
   channel <- newChan'
   forkIO $ forever $ producer1 channel
   forkIO $ forever $ producer2 channel
   forever $ reader channel

producer1 :: Channel Int -> IO ()
producer1 channel = do 
    putChan channel 6
    threadDelay 5000000

producer2 :: Channel Int -> IO ()
producer2 channel = do 
    putChan channel 8
    threadDelay 2000000

reader :: Channel Int -> IO ()
reader channel = do 
    x <- getChan channel
    putStrLn $ "read value: " ++ show x

-- dupChan :: Channel a -> IO (Channel a)
-- dupChan (head, tail) = do
--     newHead <- newEmptyMVar
--     end <- readMVar tail
--     putMVar newHead end
--     return (newHead, tail)
