module Web.Helpers where

import Data.UUID (fromText)
import Web.Scotty.Trans (ActionT)

import DB.User (UserId (UserId))
import Web.Sessions (getAssign, insertAssign, modifySession, readSession)
import Web.Types (WebEnvironment (sessions), WebM)

isUserAuthenticated :: ActionT LText WebM Bool
isUserAuthenticated = do
  mUserAssigns <- readSession =<< asks sessions
  case mUserAssigns of
    Nothing -> pure False
    Just ua -> do
      let content = getAssign "authed?" ua
      if content == Just "true"
      then pure True
      else pure False

markUserAsAuthenticated :: UserId -> ActionT LText WebM ()
markUserAsAuthenticated userId = do
  sm <- asks sessions
  modifySession sm (\mVal -> mVal >>= Just . insertAssign "authed?" "true")
  modifySession sm (\mVal -> mVal >>= Just . insertAssign "user_id" (toText userId))

getUserId :: ActionT LText WebM (Maybe UserId)
getUserId = do
  mUserAssigns <- readSession =<< asks sessions
  case mUserAssigns of
    Nothing -> pure Nothing
    Just ua ->
      pure $ UserId <$> (fromText =<< getAssign "user_id" ua)
