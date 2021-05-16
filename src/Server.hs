module Server where

import Colourista.IO (greenMessage)
import Foundation
import Handler ()
import Yesod.Core
import qualified Yesod.Core.Types as YT

import Control.Monad.Logger (liftLoc)
import Data.Default.Class
import Database.PostgreSQL.Entity.DBT (mkPool)
import Environment
import Language.Haskell.TH.Syntax (qLocation)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings,
                                 defaultShouldDisplayException, runSettings,
                                 setOnException, setPort)
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import Network.Wai.Middleware.Static (noDots, staticPolicy)
import System.Log.FastLogger (LoggerSet, defaultBufSize, newStdoutLoggerSet,
                              toLogStr)
import Web.Middleware (heartbeat)
import Web.Sessions.Server (createSessionManager)

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: MatchmakerEnv -> IO Foundation
makeFoundation MatchmakerEnv{..} = do
    let PoolConfig{..} = matchmakerPoolConfig
        appPort = matchmakerHttpPort
        appLogLevel = matchmakerLogLevel
        appRoot = Just "/"
    appPgPool <- mkPool matchmakerPgConfig subPools connectionTimeout connections
    appHttpManager <- getGlobalManager

    -- TODO(jonathan): If we want to add structured logging we should do it here
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appSessionManager <- createSessionManager

    return Foundation {..}
  where
    makeYesodLogger :: LoggerSet -> IO YT.Logger
    makeYesodLogger loggerSet' = do
        (getter, _) <- clockDateCacher
        return $! YT.Logger loggerSet' getter

makeMiddleware :: Foundation -> IO Middleware
makeMiddleware foundation = do
  logWare <- makeLogWare foundation
  pure
    $ logWare
    . defaultMiddlewaresNoLogging
    . heartbeat
    . simpleCors
    . staticPolicy noDots

-- TODO(jonathan): We probably weant to be more considerate about our log levels
-- but this will do for now
makeLogWare :: Foundation -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if LevelError <= appLogLevel foundation
                then Detailed True
                else Apache FromSocket
        , destination = Logger $ YT.loggerSet $ appLogger foundation
        }

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeWaiApplication :: Foundation -> IO Application
makeWaiApplication foundation = do
    matchmakerMiddleware <- makeMiddleware foundation
    appPlain <- toWaiAppPlain foundation
    return $ matchmakerMiddleware appPlain

-- | Warp settings for the given foundation value.
warpSettings :: Foundation -> Settings
warpSettings foundation =
      setPort (fromIntegral $ appPort foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
-- TODO(jonathan): Need to get this working
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getMatchmakerEnv
    foundation <- makeFoundation settings
    let wsettings = warpSettings foundation
    app <- makeWaiApplication foundation
    return (wsettings, app)

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from the environment
    matchmakerEnv@MatchmakerEnv{..} <- getMatchmakerEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation matchmakerEnv

    -- Generate a WAI Application from the foundation
    app <- makeWaiApplication foundation

    greenMessage
       $ "Running λ💜 Matchmaker 💜λ on port "
      <> show matchmakerHttpPort
      <> " loglevel is "
      <> show matchmakerLogLevel

    -- Run the application with Warp
    runSettings (warpSettings foundation) app

