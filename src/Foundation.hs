{-# LANGUAGE TypeFamilies #-}
module Foundation where

import DB
import Database.PostgreSQL.Entity.DBT (ConnectionPool)
import qualified Database.PostgreSQL.Entity.DBT as ENT
import Database.PostgreSQL.Transact (DBT)
import Network.HTTP.Client (HasHttpManager (..), Manager)
import Prelude hiding (get)
import Web.Router (router)
import Web.Sessions.Server (SessionManager)
import Web.Sessions.Types (UserAssigns)
import Yesod.Core
import Yesod.Core.Types

-- | Foundation is the core data type for our application, it is available as a reader argument
-- in the @Handler@ monad (which is created by mkYesodData). Foundation can be accessed by calling
-- the helper function 'getYesod'. This is where things like in memory sessions, db pools, and
-- http managers should be stored. Generally environment variables are parsed at startup and
-- put into this data type.
data Foundation = Foundation
    { appPgPool         :: ConnectionPool
    , appRoot           :: Maybe Text
    , appPort           :: Word16
    , appHttpManager    :: Manager
    , appLogger         :: Logger
    , appLogLevel       :: LogLevel
    , appSessionManager :: SessionManager UserAssigns
    }

-- | This is a magical piece of template haskell that does 1/2 of the routing work for yesod.
-- mkYesodData takes the @router@ (this is traditionally defined in a yesodroutes file, but we
-- have opted to use the eDSL rowdy) and turns the router into data types for each handler
-- function that is required. The other 1/2 of the routing magic takes your defined handler
-- functions and gives yesod access to them, this can be found in 'src/Handler.hs'.
-- N.B. A Handler loosely translates to endpoint in yesod-land
mkYesodData "Foundation" router

instance YesodBreadcrumbs Foundation where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route Foundation  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route Foundation))
    breadcrumb HomeR          = return ("Home", Nothing)
    breadcrumb LoginR         = return ("Login", Just HomeR)
    breadcrumb LoginSigninR   = return ("Signin", Just LoginR)
    breadcrumb SignupR        = return ("Signup", Just HomeR)
    breadcrumb AccountCreateR = return ("Account Creation", Just HomeR)
    breadcrumb UserR          = return ("User", Just HomeR)

-- This is potentially useful for generically establishing http connections
instance HasHttpManager Foundation where
  getHttpManager = appHttpManager

instance Yesod Foundation where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot Foundation
    approot = ApprootRequest $ \app req ->
        case appRoot app of
            Nothing   -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: Foundation -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware handler = do
      addHeader "Vary" "Accept, Accept-Language"
      addHeader "X-XSS-Protection" "1; mode=block"
      authorizationCheck
      handler

    isAuthorized
        :: Route Foundation  -- ^ The route the user is visiting.
        -> Bool              -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- What messages should be logged. Currently we just log everything, but eventually
    -- we will probably want to branch on environment and loglevel
    shouldLogIO :: Foundation -> Text -> LogLevel -> IO Bool
    shouldLogIO _ _ _ = return True

    makeLogger :: Foundation -> IO Logger
    makeLogger = return . appLogger

-- Handler

-- This is a convenience type that accomplishes two things:
--  1. It allows us to factor out some common boiler plate
--  2. It allows us to change DB implementations per monad in the future
instance HasDB Handler where
  runDB :: DBT IO a -> Handler a
  runDB dbAction = do
    Foundation{..} <- getYesod
    liftIO $ ENT.runDB appPgPool dbAction

instance MonadFail Handler where
  fail = liftIO . fail
