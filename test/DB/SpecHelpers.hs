module DB.SpecHelpers where

import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Migration

migrate :: Connection -> IO ()
migrate conn = void $ runMigrations False conn [MigrationInitialization, MigrationDirectory "./migrations"]
