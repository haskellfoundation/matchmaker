module Web.Controller.Home where

import Web.Scotty.Trans (ActionT, html)

import Web.Types (WebM)
import qualified Web.View.Home as Home

index :: ActionT LText WebM ()
index = do
  result <- Home.index
  html result

