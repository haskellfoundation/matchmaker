module Web.Auth
  ( isUserAuthenticated
  , getUserIdFromSession
  , markUserAsAuthenticated
  , withUserId
  , authenticationMiddleware
  ) where

import Data.UUID (fromText)
import Web.Scotty.Trans (ActionT)

import DB.User (UserId (..))
import Network.Wai (Middleware)
import Network.Wai.Middleware.Auth
import Web.Sessions (fetchAssign, getAssign, insertAssign, modifySession,
                     putAssign, readSession)
import Web.Types (MatchmakerError, MatchmakerError, WebEnvironment (sessions),
                  WebM, WebM)

isUserAuthenticated :: ActionT MatchmakerError WebM Bool
isUserAuthenticated = do
  result <- fetchAssign "authed?"
  if result == Just "true"
  then pure True
  else pure False

markUserAsAuthenticated :: UserId -> ActionT MatchmakerError WebM ()
markUserAsAuthenticated userId =
  putAssign "user_id" (toText userId)

getUserIdFromSession :: ActionT MatchmakerError WebM (Maybe UserId)
getUserIdFromSession = do
  result <- fetchAssign "user_id"
  case result of
    Nothing -> pure Nothing
    Just uuid -> pure $ UserId <$> fromText uuid

withUserId :: (UserId -> ActionT MatchmakerError WebM ())
           -> ActionT MatchmakerError WebM ()
withUserId action = do
  result <- getUserIdFromSession
  whenJust result action

authenticationMiddleware :: IO Middleware
authenticationMiddleware = mkAuthMiddleware settings
  where
    settings = defaultAuthSettings
