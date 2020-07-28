module ConcurrencyKie (
    runServer
) where

import Network.Socket
import Control.Monad

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
acceptConnections socket = forever' $ acceptConnection socket

acceptConnection :: Socket -> IO ()
acceptConnection socket = do 
    (csock, caddr) <- accept socket
    putStrLn $ show caddr
    close csock
    


forever' :: IO a -> IO ()
forever' action = do
    result <- action
    forever' action

