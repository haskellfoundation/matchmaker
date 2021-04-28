{-# OPTIONS_GHC -Wno-unused-imports #-}
module Web.Router where

import Prelude hiding (get)

import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
import Web.Scotty.Trans (ScottyT, defaultHandler, get, middleware, post)

import qualified Web.Controller.Home as Home
import qualified Web.Controller.Session as Session
import Web.Helpers
import Web.Types

router :: HasCallStack => ScottyT MatchmakerError WebM ()
router = do
  middleware logStdoutDev
  middleware $ staticPolicy noDots
  middleware simpleCors
  defaultHandler errorHandler

  get "/"              Home.index
  get "/login"         Session.new
  post "/login/signin" Session.create
