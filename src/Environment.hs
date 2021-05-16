{-# LANGUAGE StrictData #-}
module Environment
  ( MatchmakerEnv (..)
  , PoolConfig(..)
  , getMatchmakerEnv
  ) where


import Control.Monad.Logger (LogLevel (..))
import Data.Time (NominalDiffTime)
import qualified Database.PostgreSQL.Simple as PG
import Env (AsUnread (unread), Error (..), Parser, Reader, help, parse, str,
            var)
import Prelude hiding (Reader)

data MatchmakerEnv
  = MatchmakerEnv { matchmakerPgConfig   :: PG.ConnectInfo
                  , matchmakerPoolConfig :: PoolConfig
                  , matchmakerHttpPort   :: Word16
                  , matchmakerLogLevel   :: LogLevel
                  }
  deriving (Show)

data PoolConfig
  = PoolConfig { subPools          :: Int
               , connectionTimeout :: NominalDiffTime
               , connections       :: Int
               }
  deriving (Show)


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

parsePort :: Parser Error Word16
parsePort = var port "MATCHMAKER_PORT" (help "HTTP Port for Matchmaker")

parseLogLevel :: Parser Error LogLevel
parseLogLevel = var readLogLevel "MATCHMAKER_LOG_LEVEL" (help "Log level for Matchmaker")

parseConfig :: Parser Error MatchmakerEnv
parseConfig =
  MatchmakerEnv
    <$> parseConnectInfo
    <*> parsePoolConfig
    <*> parsePort
    <*> parseLogLevel

getMatchmakerEnv :: IO MatchmakerEnv
getMatchmakerEnv = Env.parse id parseConfig

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

readLogLevel :: Reader Error LogLevel
readLogLevel ll = do
  ll' <- str ll
  case ll' of
    "debug"  -> Right LevelDebug
    "info"   -> Right LevelInfo
    "warn"   -> Right LevelWarn
    "error"  -> Right LevelError
    "silent" -> Right $ LevelOther "silent"
    loglevel -> Left . unread $ loglevel <> " is not a valid option for MATCHMAKER_LOG_LEVEL"
