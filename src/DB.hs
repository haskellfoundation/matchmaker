module DB where

import Database.PostgreSQL.Transact (DBT)

class HasDB m where
    runDB :: DBT IO a -> m a
