{-# LANGUAGE StrictData #-}
module Web.Types
  ( WebM (..)
  , WebEnvironment (..)
  , MatchmakerError(..)
  , DBError(..)
  , WebError(..)
  , runWebM
  ) where

import Database.PostgreSQL.Entity.DBT (ConnectionPool)
import Web.Scotty.Trans (ScottyError (..))
import Web.Sessions (ScottySM, UserAssigns)

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
