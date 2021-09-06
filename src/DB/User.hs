{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-redundant-constraints #-}
module DB.User where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Password.Argon2 (Argon2, Password, PasswordCheck (..), PasswordHash)
import qualified Data.Password.Argon2 as Argon2
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Transact (DBT)
import GHC.TypeLits (ErrorMessage (..), TypeError)

newtype UserId
  = UserId { getUserId :: UUID }
  deriving stock (Eq, Generic)
  deriving newtype (FromField, FromJSON, Show, ToField, ToJSON)

instance ToText UserId where
  toText (UserId uuid) = UUID.toText uuid

data User
  = User { userId      :: UserId
         , username    :: Text
         , email       :: Text
         , displayName :: Text
         , password    :: PasswordHash Argon2
         , createdAt   :: UTCTime
         , updatedAt   :: UTCTime
         }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "users"] User)

-- | Type error! Do not use 'toJSON' on a 'Password'!
instance TypeError (ErrMsg "JSON") => ToJSON Password where
  toJSON = error "unreachable"

type ErrMsg e = 'Text "Warning! Tried to convert plain-text Password to " ':<>: 'Text e ':<>: 'Text "!"
          ':$$: 'Text "  This is likely a security leak. Please make sure whether this was intended."
          ':$$: 'Text "  If this is intended, please use 'unsafeShowPassword' before converting to " ':<>: 'Text e
          ':$$: 'Text ""

instance FromJSON Password where
  parseJSON = fmap Argon2.mkPassword . parseJSON

deriving via Text instance ToField (PasswordHash a)
deriving via Text instance FromField (PasswordHash a)

-- Database operations

hashPassword :: (MonadIO m) => Password -> m (PasswordHash Argon2)
hashPassword = Argon2.hashPassword

validatePassword :: Password -> PasswordHash Argon2 -> Bool
validatePassword inputPassword hashedPassword =
  Argon2.checkPassword inputPassword hashedPassword == PasswordCheckSuccess

insertUser :: HasCallStack => User -> DBT IO ()
insertUser user = insert @User user

getUserById :: HasCallStack => UserId -> DBT IO (Maybe User)
getUserById userId = selectById (Only userId)

getUserByUsername :: HasCallStack => Text -> DBT IO (Maybe User)
getUserByUsername username = selectOneByField [field| username |] (Only username)

getUserByEmail :: HasCallStack => Text -> DBT IO (Maybe User)
getUserByEmail email = selectOneByField [field| email |] (Only email)

deleteUser :: HasCallStack => UserId -> DBT IO ()
deleteUser userId = delete @User (Only userId)
