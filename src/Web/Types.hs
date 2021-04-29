{-# LANGUAGE StrictData #-}
module Web.Types
  ( DBError(..)
  , MatchmakerError(..)
  , ScottySM (..)
  , Session (..)
  , SessionJar
  , UserAssigns(..)
  , WebEnvironment (..)
  , WebError(..)
  , WebM (..)
  , runWebM
  ) where

import Data.Time (UTCTime)
import Database.PostgreSQL.Entity.DBT (ConnectionPool)
import Web.Scotty.Trans (ScottyError (..))

newtype WebM a
  = WebM { getWeb :: ReaderT WebEnvironment IO a }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader WebEnvironment)

data WebEnvironment
  = WebEnvironment { sessions      :: ScottySM UserAssigns
                   , pgPool        :: ConnectionPool
                   , templateCache :: HashMap String String
                   }

runWebM :: WebEnvironment -> WebM a -> IO a
runWebM env action =
  runReaderT (getWeb action) env

data MatchmakerError
  = DB DBError
  | Web WebError
  | TextError {-# UNPACK #-}Text
  deriving stock (Eq, Generic, Show)

data WebError
  = LoginFailure
  deriving stock (Eq, Generic, Show)

instance Exception WebError

data DBError
  = ConstraintError {-# UNPACK #-}Text
  | NotFound
  | TooManyResults
  | InsertionError
  | DeserialisationError {-# UNPACK #-}Text
  deriving stock (Eq, Generic, Show)

instance Exception DBError

instance ScottyError MatchmakerError where
  stringError :: String -> MatchmakerError
  stringError = TextError . toText
  showError :: MatchmakerError -> LText
  showError = show

data Session a =
  Session { sess_id         :: Text
          , sess_validUntil :: UTCTime
          , sess_content    :: Maybe a
          } deriving (Show, Eq)

type SessionJar a = TVar (HashMap Text (Session a))

newtype ScottySM a =
  ScottySM { _unSessionManager :: SessionJar a }
  deriving stock (Eq)

newtype UserAssigns = UserAssigns { getUserAssigns :: HashMap Text Text }
  deriving newtype (Show, Eq)

