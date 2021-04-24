{-# LANGUAGE OverloadedLists #-}
module DB.Repository where

import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField ( FromField )
import Data.Aeson ( FromJSON, ToJSON )
import Database.PostgreSQL.Simple.ToField ( ToField )
import Database.PostgreSQL.Simple ( ToRow, FromRow, Only(Only) )
import Data.Time (UTCTime)
import Database.PostgreSQL.Entity
    ( insert, selectById, selectOneByField, Entity(..), delete, selectManyByField )
import Database.PostgreSQL.Transact ( DBT )

import DB.Organisation (OrganisationId (..))
import Data.Vector (Vector)

newtype RepositoryId
  = RepositoryId { getRepositoryId :: UUID }
  deriving stock (Eq, Generic)
  deriving newtype (FromField, FromJSON, Show, ToField, ToJSON)

data Repository
  = Repository { repositoryId :: RepositoryId
               , organisationId :: OrganisationId
               , repositoryName :: Text
               , repositoryDescription :: Text
               , repositoryURL :: Text
               , repositoryHomepage :: Maybe Text
               , createdAt   :: UTCTime
               , updatedAt   :: UTCTime
               }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromRow, ToRow)

instance Entity Repository where
  tableName = "repositories"
  primaryKey = "repository_id"
  fields = [ "repository_id"
           , "organisation_id"
           , "repository_name"
           , "repository_description"
           , "repository_url"
           , "repository_homepage"
           , "created_at"
           , "updated_at"
           ]

insertRepository :: Repository -> DBT IO ()
insertRepository repo = insert @Repository repo

getRepository :: RepositoryId -> DBT IO Repository
getRepository repoId = selectById @Repository (Only repoId)

getRepositoriesByOrg :: OrganisationId -> DBT IO (Vector Repository)
getRepositoriesByOrg orgId = selectManyByField @Repository "organisation_id" (Only orgId)

getRepositoryByName :: Text -> DBT IO Repository
getRepositoryByName name = selectOneByField "repository_name" (Only name)

deleteRepository :: RepositoryId -> DBT IO ()
deleteRepository repoId = delete @Repository (Only repoId)
