module ReaderKie (
    main''
) where

-- import Control.Monad.Reader


data Environment = Environment { 
    url :: String    
}

env = Environment {
    url = "http://localhost/myComics"
}

clientHttp "http://localhost/myComics" = "B1 B2 B3"
clientHttp _ =  error "Wrong url"

download :: Environment -> [String]
download env = let responseBody = clientHttp (url env) in
    words responseBody

doSomethingsAndDownload :: Environment -> String -> [String]
doSomethingsAndDownload env "fooValue" = 
    --some calculations
    download env

main = doSomethingsAndDownload env "fooValue"


main' = doSomethingsAndDownload' "fooValue" env

doSomethingsAndDownload' :: String -> (Environment -> [String])
doSomethingsAndDownload' "fooValue" = 
    --some calculations
    download


newtype Reader e a = Reader {runReader :: e -> a}

main'' = runReader (doSomethingsAndDownload'' "fooValue") env

doSomethingsAndDownload'' :: String -> Reader Environment [String]
doSomethingsAndDownload'' fooValue = download'''

-- download'' :: Reader Environment [String]
-- download'' = Reader (\env -> let env' = runReader ask env
--                                  responseBody = clientHttp (url env') in            
--                              words responseBody)

download''' :: Reader Environment [String]
download''' = Reader (\env -> let responseBody = clientHttp (url env) in            
                             words responseBody)
                             

-- ask :: Reader Environment Environment
-- ask = Reader id

-- main'' = runReader (doSomethingsAndDownload'' "fooValue") env

-- doSomethingsAndDownload'' :: String -> Reader Environment [String]
-- doSomethingsAndDownload'' fooValue = download''

-- download'' :: Reader Environment [String]
-- -- download'' = ask >>= \env -> return $ words . clientHttp $ url env
-- download'' = do 
--     env <- ask     
--     let responseBody = clientHttp $ url env
--     return $ words responseBody

