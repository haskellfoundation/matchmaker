module Server where

import Colourista.IO (greenMessage)
import qualified Data.HashMap.Strict as HM
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Web.Scotty.Trans (Options (Options), scottyOptsT)

import Environment
import Web.Router (router)
import Web.Sessions (createSessionManager)
import Web.Types (WebEnvironment (..), WebM, runWebM)

startWebService :: HasCallStack => IO ()
startWebService = do
  MatchmakerEnv{pgPool, httpPort} <- liftIO mkEnv
  greenMessage $ "[+] Starting web server on http://localhost:" <> show httpPort
  let templateCache = HM.empty
  sessions <- createSessionManager
  let env = WebEnvironment{..}
  scottyOptsT (serverOptions httpPort) (runIO env) router

serverOptions :: Word16 -> Options
serverOptions httpPort = Options 0 settings
  where
    settings = setPort (fromIntegral httpPort) defaultSettings

runIO :: WebEnvironment -> WebM a -> IO a
runIO env m = runWebM env m
