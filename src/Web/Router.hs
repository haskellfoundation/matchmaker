{-# OPTIONS_GHC -Wno-unused-imports #-}
module Web.Router where

import Prelude hiding (get)

import Data.Default.Class (Default (def))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (Destination (Handle), DetailedSettings (mModifyParams, useColors),
                                             OutputFormat (DetailedWithSettings),
                                             RequestLoggerSettings (autoFlush, destination, outputFormat),
                                             mkRequestLogger)
import Network.Wai.Middleware.Static (noDots, staticPolicy)
import Web.Scotty.Trans (ScottyT, defaultHandler, get, middleware, post)

import Network.HTTP.Types (status200)
import Network.Wai (Application, Middleware, Request (requestMethod), pathInfo,
                    responseLBS)
import System.IO.Unsafe (unsafePerformIO)
import qualified Web.Controller.Account as Account
import qualified Web.Controller.Home as Home
import qualified Web.Controller.Session as Session
import qualified Web.Controller.User as User
import Web.Middleware
import Web.Scotty (Param)
import Web.Templates (errorHandler)
import Web.Types (MatchmakerError, WebM)

router :: HasCallStack => ScottyT MatchmakerError WebM ()
router = do
  middleware logger
  middleware $ staticPolicy noDots
  middleware simpleCors
  middleware heartbeat
  defaultHandler errorHandler

  get "/"              Home.index
  get "/login"         Session.new
  post "/login/signin" Session.create

  get "/signup" Account.new
  post "/account/create" Account.create

  get "/user"       User.show

logger :: Middleware
logger = unsafePerformIO $ mkRequestLogger settings
  where
    settings =
      def{ outputFormat = DetailedWithSettings detailedsettings
         , autoFlush = True
         , destination = Handle stdout
         }
    detailedsettings =
      def{ useColors = True
         , mModifyParams = Just sensitiveInfoFilter
         }
    sensitiveInfoFilter :: (ByteString, ByteString)  -> Maybe (ByteString, ByteString)
    sensitiveInfoFilter p@(k,_) = if HashSet.member k paramsToHide
                                  then Just (k, "[FILTERED]")
                                  else Just p
    paramsToHide :: HashSet ByteString
    paramsToHide = HashSet.fromList ["password", "token", "secret"]
{-# NOINLINE logger #-}
