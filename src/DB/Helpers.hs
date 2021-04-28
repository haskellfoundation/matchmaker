{-# LANGUAGE LambdaCase #-}
module DB.Helpers where

import Control.Exception.Safe
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.PostgreSQL.Entity.DBT (ConnectionPool, runDB)
import Database.PostgreSQL.Transact (DBT)
import Web.Types

runPersistence :: (MonadBaseControl IO m, MonadCatch m, Exception MatchmakerError)
               => ConnectionPool
               -> DBT m a
               -> m a
runPersistence pool action = do
  result <- try $ runDB pool action
  case result of
    Left e  -> throw (DB e)
    Right value -> pure value
