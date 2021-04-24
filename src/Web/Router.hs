{-# OPTIONS_GHC -Wno-unused-imports #-}
module Web.Router where

import Prelude hiding (get)

import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
import Web.Scotty.Trans (ScottyT, get, middleware, post)

import qualified Web.Controller.Home as Home
import Web.Types

router :: ScottyT LText WebM ()
router = do
  middleware logStdoutDev
  middleware $ staticPolicy noDots
  middleware simpleCors

  get "/" Home.index

