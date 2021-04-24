{-# LANGUAGE StrictData #-}
module Environment
  ( MatchmakerEnv (..)
  , getConfig
  , mkEnv
  ) where

import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Entity.DBT (ConnectionPool, mkPool)
import qualified Database.PostgreSQL.Simple as PG
import Env (AsUnread (unread), Error, Parser, Reader, help, parse, str, var)
import Prelude hiding (Reader)

-- *Env datatypes are parsed as-is from the outside
data Config
  = Config { pgConfig   :: PG.ConnectInfo
           , poolConfig :: PoolConfig
           }
  deriving (Show)

data PoolConfig
  = PoolConfig { subPools          :: Int
               , connectionTimeout :: NominalDiffTime
               , connections       :: Int
               }
  deriving (Show)

getConfig :: IO Config
getConfig = Env.parse id parseConfig

parseConfig :: Parser Error Config
parseConfig =
  Config <$> parseConnectInfo
         <*> parsePoolConfig

parseConnectInfo :: Parser Error PG.ConnectInfo
parseConnectInfo =
  PG.ConnectInfo <$> var str  "DB_HOST"     (help "PostgreSQL host")
                 <*> var port "DB_PORT"     (help "PostgreSQL port")
                 <*> var str  "DB_USER"     (help "PostgreSQL user")
                 <*> var str  "DB_PASSWORD" (help "PostgreSQL password")
                 <*> var str  "DB_DATABASE" (help "Control-Plane database")

parsePoolConfig :: Parser Error PoolConfig
parsePoolConfig =
  PoolConfig <$> var (int >=> nonNegative) "DB_SUB_POOLS"        (help "Number of sub-pools")
             <*> var timeout               "DB_TIMEOUT"          (help "Timeout for each connection")
             <*> var (int >=> nonNegative) "DB_POOL_CONNECTIONS" (help "Number of connections per sub-pool")

-- *Env datatypes are the ones that are passed in the application
newtype MatchmakerEnv
  = MatchmakerEnv { pgPool :: ConnectionPool }
  deriving stock (Show)

mkEnv :: IO MatchmakerEnv
mkEnv = do
  Config{..} <- getConfig
  let PoolConfig{..} = poolConfig
  pgPool <- mkPool pgConfig subPools connectionTimeout connections
  pure $ MatchmakerEnv{..}

-- Env parser helpers

int :: Reader Error Int
int i =
  case readMaybe i of
    Nothing -> Left . unread . show $ i
    Just i' -> Right i'

port :: Reader Error Word16
port p =
  case int p of
    Left err -> Left err
    Right intPort ->
      if intPort >= 1 && intPort <= 65535
      then Right $ fromIntegral intPort
      else Left . unread . show $ p

nonNegative :: Int -> Either Error Int
nonNegative nni =
  if nni >= 0
  then Right nni
  else Left . unread . show $ nni

timeout :: Reader Error NominalDiffTime
timeout t = second fromIntegral (int >=> nonNegative $ t)
