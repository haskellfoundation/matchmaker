module Web.Middleware where

import Network.HTTP.Types (status200)
import Network.Wai (Middleware, Request (..), pathInfo, responseLBS)

heartbeat :: Middleware
heartbeat app req sendResponse = app req $ \res ->
  if method `elem` ["GET", "HEAD"] && path == ["heartbeat"]
  then sendResponse $ responseLBS status200 [("Content-Type", "text/plain")] "OK."
  else sendResponse res
    where
      method = requestMethod req
      path = pathInfo req
