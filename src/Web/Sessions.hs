module Web.Sessions
  ( UserAssigns (..),
    module Web.Sessions,
  )
where

import DB.User (UserId (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.UUID as UUID
import Foundation
import Prelude
import Web.Sessions.Server (Session (..), getServerSession, modifyServerSession,
                            upsertServerSession)
import Web.Sessions.Types (UserAssigns (..))
import Yesod.Core

lookupClientSession :: MonadHandler m => Text -> m (Maybe Text)
lookupClientSession = lookupSession

setClientSession :: MonadHandler m => Text -> Text -> m ()
setClientSession = setSession

getClientSessions :: MonadHandler m => m SessionMap
getClientSessions = getSession

clearClientSession :: MonadHandler m => Text -> m ()
clearClientSession = deleteSession

clientSessionIdentifier :: Text
clientSessionIdentifier = "sid"

markAuthenticated :: UserId -> Handler ()
markAuthenticated uId = putAssign "user_id" (toText uId)

getUserIdFromSession :: Handler (Maybe UserId)
getUserIdFromSession = readAssign "user_id" (fmap UserId . UUID.fromText)

getAllUserAssigns :: Handler (Maybe UserAssigns)
getAllUserAssigns = do
  Foundation {appSessionManager} <- getYesod
  mClientSession <- lookupSession clientSessionIdentifier
  liftIO . fmap join $
    forM mClientSession $ \sid -> do
      mSession <- getServerSession appSessionManager sid
      pure $ sessionContent <$> mSession

putAssign :: Text -> Text -> Handler ()
putAssign key value = do
  Foundation {appSessionManager} <- getYesod
  mClientSid <- lookupSession clientSessionIdentifier

  serverSession <-
    liftIO $
      upsertServerSession appSessionManager mClientSid (upsertUserAssigns key value)

  void $ setClientSession clientSessionIdentifier (UUID.toText $ sessionId serverSession)


readAssign :: Text -> (Text -> Maybe a) -> Handler (Maybe a)
readAssign key f = do
  assign <- fetchAssign key
  pure $ f =<< assign

fetchAssign :: Text -> Handler (Maybe Text)
fetchAssign key = do
  Foundation {appSessionManager} <- getYesod
  mClientSession <- lookupSession clientSessionIdentifier
  liftIO . fmap join $
    forM mClientSession $ \sid -> do
      mSession <- getServerSession appSessionManager sid
      pure $ mSession >>= lookupUserAssign key . sessionContent

popAssign :: Text -> Handler (Maybe Text)
popAssign key = do
  Foundation {appSessionManager} <- getYesod
  mClientSession <- lookupSession clientSessionIdentifier

  liftIO . fmap join $
    forM mClientSession $ \sid -> do
      mSession <- getServerSession appSessionManager sid
      let mAssign = mSession >>= lookupUserAssign key . sessionContent
      forM mAssign $ \assign ->
        modifyServerSession appSessionManager sid (removeUserAssign key) >> pure assign

upsertUserAssigns :: Text -> Text -> Maybe (Session UserAssigns) -> UserAssigns
upsertUserAssigns key value Nothing = UserAssigns $ HM.insert key value HM.empty
upsertUserAssigns key value (Just Session {..}) =
  UserAssigns
    . HM.insert key value
    . getUserAssigns
    $ sessionContent

removeUserAssign :: Text -> UserAssigns -> UserAssigns
removeUserAssign key = UserAssigns . HM.delete key . getUserAssigns

lookupUserAssign :: Text -> UserAssigns -> Maybe Text
lookupUserAssign key = HM.lookup key . getUserAssigns
