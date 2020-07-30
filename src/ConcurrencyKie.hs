module ConcurrencyKie (
    runServer
) where

import Network.Socket
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent

addr = SockAddrInet 8080 $ tupleToHostAddress (127, 0, 0, 1)

runServer = do 
    socket <- initSocket
    acceptConnections socket

initSocket :: IO Socket
initSocket = do 
    sock <- socket AF_INET  Stream defaultProtocol 
    bind sock addr
    listen sock 5
    return sock

acceptConnections :: Socket -> IO ()
acceptConnections socket = do
    count <- newEmptyMVar
    putMVar count 0
    forever $ do 
        (csock, caddr) <- accept socket
        forkIO (serveConnection csock count)

serveConnection :: Socket -> MVar Int -> IO ()
serveConnection socket count = do     
    increment count    
    putStrLn " (sleep 5s)"
    threadDelay 5000000
    close socket
    decrement count

increment :: MVar Int -> IO ()
increment count = do 
    value <- takeMVar count 
    putMVar count (value+1)

decrement :: MVar Int -> IO ()
decrement count = do 
    value <- takeMVar count 
    putMVar count (value-1)

