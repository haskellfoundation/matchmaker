{-# LANGUAGE OverloadedLists #-}
module DB.ContributorCall where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Entity (Entity (..), delete, insert, selectById)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)

import DB.Repository (RepositoryId)

newtype ContributorCallId
  = ContributorCallId { getContributorCallId :: UUID }
  deriving stock (Eq, Generic)
  deriving newtype (FromField, FromJSON, Show, ToField, ToJSON)

data ContributorCall
  = ContributorCall { contributorCallId :: ContributorCallId
                    , repositoryId      :: RepositoryId
                    , title             :: Text
                    , description       :: Text
                    , createdAt         :: UTCTime
                    , updatedAt         :: UTCTime
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

getContributorCall :: ContributorCallId -> DBT IO (Maybe ContributorCall)
getContributorCall ccId = selectById @ContributorCall (Only ccId)

deleteContributorCall :: ContributorCallId -> DBT IO ()
deleteContributorCall ccId = delete @ContributorCall (Only ccId)
