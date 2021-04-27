module Server where

import Colourista.IO (greenMessage)
import qualified Data.HashMap.Strict as HM
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Web.Scotty.Trans (Options (Options), scottyOptsT)

import Environment (MatchmakerEnv (..), mkEnv)
import Web.Router (router)
import Web.Sessions (createSessionManager)
import Web.Types (WebEnvironment (..), WebM, runWebM)

startWebService :: IO ()
startWebService = do
  greenMessage "[+] Starting web server on http://localhost:8008"
  MatchmakerEnv{pgPool} <- mkEnv
  let templateCache = HM.empty
  sessions <- createSessionManager
  let env = WebEnvironment{..}
  scottyOptsT serverOptions (runIO env) router

serverOptions :: Options
serverOptions = Options 0 settings
  where
    settings = setPort 8008 defaultSettings

runIO :: WebEnvironment -> WebM a -> IO a
runIO env m = runWebM env m
