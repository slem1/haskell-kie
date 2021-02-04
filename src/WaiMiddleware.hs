{-# LANGUAGE OverloadedStrings #-}

module WaiMiddleware(
    runWebApp
)
where


import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai.Handler.Warp

import Data.Monoid (mconcat)

runWebApp = run 3000 $ addLog $ addJsonContentType $ serve200Application    

serve200Application :: Application
serve200Application request responseFn = 
    let response = responseLBS status200 [] "{ \"message\" : \"hello this is a 200 text\" }" in
        responseFn response

addJsonContentType :: Middleware
addJsonContentType baseApp request responseFn =   
    let addJsonResponseHeader responseHeaders = (hContentType, "application/json"):responseHeaders  
        responseFn' response = responseFn $ mapResponseHeaders addJsonResponseHeader response  in
            baseApp request responseFn'

addLog :: Middleware
addLog baseApp request responseFn = baseApp request responseFn' 
    where
        responseFn' response = do
            putStrLn "Log a response"
            responseFn response 


-- myMiddleware :: Middleware
-- myMiddleware baseApp = serve200Application

-- myMiddleware :: Middleware
-- myMiddleware baseApp request responseFn = serve200Application request responseFn