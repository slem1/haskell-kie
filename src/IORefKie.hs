module IORefKie (
    readAndWrite
) where

import Data.IORef


--readAndWrite
readAndWrite = do
    mutable <- newIORef 0
    currentValue <- readIORef mutable
    writeIORef mutable (currentValue + 1)
    updatedValue <- readIORef mutable
    putStrLn $ show updatedValue
    


