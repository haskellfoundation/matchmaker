{-# LANGUAGE DeriveFunctor #-}

-- |
-- A session has 2 parts in the matchmaker architecture. The first is managed by yesod, which
-- is the client session. These are cookies (yesod handles encryption) that live on the users
-- browser. We keep the data on the client side minimal, it is just a @UUID@ that corresponds
-- to some state that we maintain on the server. This is the second part of the session; an in
-- memory data structure (@HM.HashMap@ for now) that keeps track of various pieces of user state.
-- The functions in this section are helpers for initializing, modifying, and cleaning up the
-- state that we maintain on the server.
module Web.Sessions.Server where

import Control.Concurrent
import qualified Data.HashMap.Strict as HM
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID, fromText)
import Data.UUID.V4

data Session a = Session
  { sessionId         :: UUID,
    sessionValidUntil :: UTCTime,
    sessionContent    :: a
  }
  deriving (Show, Eq, Functor)

type SessionManager a = TVar (HashMap UUID (Session a))

sessionTTL :: NominalDiffTime
sessionTTL = 36000

makeSession :: a -> IO (Session a)
makeSession content = do
  uuid <- nextRandom
  now <- getCurrentTime
  let validUntil = addUTCTime sessionTTL now
  pure $ Session uuid validUntil content

insertServerSession :: SessionManager a -> Session a -> IO ()
insertServerSession sessions sessionVal  =
  atomically $ modifyTVar' sessions $ HM.insert (sessionId sessionVal) sessionVal

getServerSession :: SessionManager a -> Text -> IO (Maybe (Session a))
getServerSession sessions sid =
  fmap join $ forM (fromText sid) $ \sessionId -> do
    s <- readTVarIO sessions
    pure $ HM.lookup sessionId s

upsertServerSession ::
  SessionManager a ->
  Maybe Text ->
  (Maybe (Session a) -> a) ->
  IO (Session a)
upsertServerSession manager mSid f = do
  mSessionAssigns <- join <$> traverse (getServerSession manager) mSid
  s <- makeSession . f $ mSessionAssigns
  insertServerSession manager s
  pure s

modifyServerSession ::
  SessionManager a ->
  Text ->
  (a -> a) ->
  IO (Maybe (Session a))
modifyServerSession manager sid f = do
  mSessionAssigns <- getServerSession manager sid
  forM mSessionAssigns $ \sessionAssigns -> do
    s <- makeSession . f $ sessionContent sessionAssigns
    insertServerSession manager s
    pure s

createSessionManager :: IO (SessionManager a)
createSessionManager = do
  storage <- newTVarIO HM.empty
  forkIO $ maintainServerSessions storage
  pure storage

maintainServerSessions :: SessionManager a -> IO ()
maintainServerSessions sessions = do
  now <- getCurrentTime
  atomically $ modifyTVar' sessions $ \m -> HM.filter (stillValid now) m
  threadDelay 1000000
  maintainServerSessions sessions
  where
    stillValid currTime sess = sessionValidUntil sess > currTime

