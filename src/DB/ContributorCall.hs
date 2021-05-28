module DB.ContributorCall where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)

import DB.Repository (RepositoryId)
import Database.PostgreSQL.Entity.Types

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
    deriving (Entity)
      via (GenericEntity '[TableName "contributor_calls"] ContributorCall)

insertContributorCall :: ContributorCall -> DBT IO ()
insertContributorCall cc = insert @ContributorCall cc

getContributorCall :: ContributorCallId -> DBT IO (Maybe ContributorCall)
getContributorCall ccId = selectById @ContributorCall (Only ccId)

deleteContributorCall :: ContributorCallId -> DBT IO ()
deleteContributorCall ccId = delete @ContributorCall (Only ccId)
