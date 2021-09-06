{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module DB.Organisation where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)

import DB.User (User, UserId)
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, queryOne,
                                       query_)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.SqlQQ (sql)

newtype OrganisationId
  = OrganisationId { getOrganisationId :: UUID }
  deriving stock (Eq, Generic)
  deriving newtype (FromField, FromJSON, Show, ToField, ToJSON)

data Organisation
  = Organisation { organisationId   :: OrganisationId
                 , organisationName :: Text
                 , createdAt        :: UTCTime
                 , updatedAt        :: UTCTime
                 }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromRow, ToRow)
    deriving (Entity)
      via (GenericEntity '[TableName "organisations"] Organisation)

newtype UserOrganisationId
  = UserOrganisationId { getUserOrganisationId :: UUID }
  deriving stock (Eq, Generic)
  deriving newtype (FromField, FromJSON, Show, ToField, ToJSON)

data UserOrganisation
  = UserOrganisation { userOrganisationId :: UserOrganisationId
                     , userId             :: UserId
                     , organisationId     :: OrganisationId
                     , isAdmin            :: Bool
                     }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromRow, ToRow)
    deriving Entity
      via (GenericEntity '[TableName "user_organisation"] UserOrganisation)

insertOrganisation :: Organisation -> DBT IO ()
insertOrganisation org = insert @Organisation org

getOrganisationById :: OrganisationId -> DBT IO (Maybe Organisation)
getOrganisationById orgId = selectById @Organisation (Only orgId)

getOrganisationByName :: Text -> DBT IO (Maybe Organisation)
getOrganisationByName name = selectOneByField [field| organisation_name |] (Only name)

deleteOrganisation :: OrganisationId -> DBT IO ()
deleteOrganisation orgId = delete @Organisation (Only orgId)

getAllUserOrganisations :: DBT IO (Vector UserOrganisation)
getAllUserOrganisations = query_ Select (_select @UserOrganisation)

getUserOrganisationById :: UserOrganisationId -> DBT IO (Maybe UserOrganisation)
getUserOrganisationById uoId = selectById @UserOrganisation (Only uoId)

getUserOrganisation :: UserId -> OrganisationId -> DBT IO (Maybe UserOrganisation)
getUserOrganisation userId orgId = queryOne Select q (userId, orgId)
    where q = _selectWhere @UserOrganisation [[field| user_id |], [field| organisation_id |]]

makeAdmin :: UserId -> OrganisationId -> DBT IO ()
makeAdmin userId organisationId = do
  uo <- fromJust <$> getUserOrganisation userId organisationId
  let newUO = uo{isAdmin = True}
  update @UserOrganisation newUO

attachUser :: UserId -> OrganisationId -> UserOrganisationId -> DBT IO ()
attachUser userId organisationId uoId = do
  insert @UserOrganisation (UserOrganisation uoId userId organisationId False)

getUsers :: OrganisationId -> DBT IO (Vector User)
getUsers orgId = query Select q (Only orgId)
    where q = [sql|
        SELECT u.user_id, u.username, u.email, u.display_name, u.password, u.created_at, u.updated_at
        FROM users AS u
            JOIN user_organisation AS uo
                ON u.user_id = uo.user_id
        WHERE uo.organisation_id = ?
        |]

getAdmins :: OrganisationId -> DBT IO (Vector User)
getAdmins orgId = query Select q (Only orgId)
    where q = [sql|
        SELECT u.user_id, u.username, u.email, u.display_name, u.password, u.created_at, u.updated_at
        FROM users AS u
            JOIN user_organisation AS uo
                ON uo.user_id = u.user_id
        WHERE uo.organisation_id = ? AND uo.is_admin = true
        |]
