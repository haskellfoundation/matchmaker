{-# LANGUAGE OverloadedLists #-}
module DB.ContributorCall where

import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField ( FromField )
import Data.Aeson ( FromJSON, ToJSON )
import Database.PostgreSQL.Simple.ToField ( ToField )
import Database.PostgreSQL.Simple ( ToRow, FromRow, Only(Only) )
import Data.Time (UTCTime)
import Database.PostgreSQL.Entity
    ( insert, selectById, Entity(..), delete )
import Database.PostgreSQL.Transact ( DBT )

import DB.Repository (RepositoryId)

newtype ContributorCallId
  = ContributorCallId { getContributorCallId :: UUID }
  deriving stock (Eq, Generic)
  deriving newtype (FromField, FromJSON, Show, ToField, ToJSON)

data ContributorCall
  = ContributorCall { contributorCallId :: ContributorCallId
                    , repositoryId :: RepositoryId
                    , title :: Text
                    , description :: Text
                    , createdAt   :: UTCTime
                    , updatedAt   :: UTCTime
                    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromRow, ToRow)

instance Entity ContributorCall where
  tableName = "contributor_calls"
  primaryKey = "contributor_call_id"
  fields = [ "contributor_call_id"
           , "repository_id"
           , "title"
           , "description"
           , "created_at"
           , "updated_at"
           ]

insertContributorCall :: ContributorCall -> DBT IO ()
insertContributorCall cc = insert @ContributorCall cc

getContributorCall :: ContributorCallId -> DBT IO ContributorCall
getContributorCall ccId = selectById @ContributorCall (Only ccId)

deleteContributorCall :: ContributorCallId -> DBT IO ()
deleteContributorCall ccId = delete @ContributorCall (Only ccId)
