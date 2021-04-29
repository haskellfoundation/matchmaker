module Web.Auth
  ( isUserAuthenticated
  , getUserIdFromSession
  , markUserAsAuthenticated
  , withUserId
  , authenticationMiddleware
  ) where

import Data.UUID (fromText)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Auth
import Web.Scotty.Trans (ActionT)

import DB.User (UserId (..))
import Web.Sessions (getAssign, insertAssign, modifySession, readSession)
import Web.Types (MatchmakerError, WebEnvironment (sessions), WebM)

isUserAuthenticated :: ActionT MatchmakerError WebM Bool
isUserAuthenticated = do
  mUserAssigns <- readSession =<< asks sessions
  case mUserAssigns of
    Nothing -> pure False
    Just ua -> do
      let content = getAssign "authed?" ua
      if content == Just "true"
      then pure True
      else pure False

markUserAsAuthenticated :: UserId -> ActionT MatchmakerError WebM ()
markUserAsAuthenticated userId = do
  sm <- asks sessions
  modifySession sm (\mVal -> mVal >>= Just . insertAssign "authed?" "true")
  modifySession sm (\mVal -> mVal >>= Just . insertAssign "user_id" (toText userId))

getUserIdFromSession :: ActionT MatchmakerError WebM (Maybe UserId)
getUserIdFromSession = do
  mUserAssigns <- readSession =<< asks sessions
  case mUserAssigns of
    Nothing -> pure Nothing
    Just ua ->
      pure $ UserId <$> (fromText =<< getAssign "user_id" ua)

withUserId :: (UserId -> ActionT MatchmakerError WebM ()) -> ActionT MatchmakerError WebM ()
withUserId action = do
  result <- getUserIdFromSession
  whenJust result action

authenticationMiddleware :: IO Middleware
authenticationMiddleware = mkAuthMiddleware settings
  where
    settings = defaultAuthSettings
