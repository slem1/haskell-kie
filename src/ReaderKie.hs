module ReaderKie (

) where


data Environment = Environment { 
    url :: String    
}

env = Environment {
    url = "http://localhost/myComics"
}

clientHttp url = "B1 B2 B3"

download :: Environment -> [String]
download env = let responseBody = clientHttp (url env) in
    words responseBody

doSomethingsAndDownload :: Environment -> String -> [String]
doSomethingsAndDownload env "fooValue" = 
    --some calculations
    download env

main = doSomethingsAndDownload env "fooValue"


main' = (doSomethingsAndDownload' "fooValue") env

doSomethingsAndDownload' :: String -> (Environment -> [String])
doSomethingsAndDownload' "fooValue" = 
    --some calculations
    download


newtype Reader e a = Reader {runReader :: (e -> a)}

main'' = runReader (doSomethingsAndDownload' "fooValue") env

doSomethingsAndDownload'' :: String -> Reader Environment [String]
doSomethingsAndDownload'' "fooValue" =     
    download

download'' :: Reader Environment [String]
download'' env = let responseBody = clientHttp (url env) in
    words responseBody












