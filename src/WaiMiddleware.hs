{-# LANGUAGE OverloadedStrings #-}

module WaiMiddleware(
    runWebApp
)
where


import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai.Handler.Warp
import Data.List
import qualified Data.ByteString as BS

import Data.Monoid (mconcat)

runWebApp = run 3000 $ addLog $ jwtInterceptor $ addJsonContentType $ serve200Application    

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

jwtInterceptor :: Middleware 
jwtInterceptor baseApp request responseFn = 
    if rawPathInfo request `elem` anonymousPaths then 
        baseApp request responseFn
    else 
        case hasAuthToken request of            
            Just (_,token) -> jwtProtectedMiddleware token baseApp request responseFn
            _ -> responseFn response401
    where
    hasAuthToken request = find (\(h,_) -> h == "auth-token") $ requestHeaders request


jwtProtectedMiddleware :: BS.ByteString -> Middleware
jwtProtectedMiddleware token baseApp request responseFn = 
    case token of 
        "12345" -> baseApp request responseFn
        _ -> responseFn response401


anonymousPaths = ["/auth/login"]

response401 = responseLBS status401 [] "{ \"message\" : \"access denied\" }"




-- myMiddleware :: Middleware
-- myMiddleware baseApp = serve200Application

-- myMiddleware :: Middleware
-- myMiddleware baseApp request responseFn = serve200Application request responseFn