module Handler.Account.Create where

import DB.Organisation
import DB.User
import Data.Time
import Data.UUID.V4 (nextRandom)
import ImportYesod
import Model.UserModel
import Web.Form
import Web.Sessions

postAccountCreateR :: Handler ()
postAccountCreateR = do
  postParams <- getPostParams
  newUser <- liftIO $ validateNewUser postParams
  case newUser of
    FieldErrors errors -> do
      putAssign "form_error" "true"
      handleFormErrors errors
      redirect SignupR
    Result user@User{..} -> do
      orgId <- liftIO $ OrganisationId <$> nextRandom
      userOrgId <- liftIO $ UserOrganisationId <$> nextRandom
      timestamp <- liftIO getCurrentTime
      let org = newOrganisationFor orgId timestamp user
      runDB
        $ insertUser user
        *> insertOrganisation org
        *> attachUser userId orgId userOrgId
      redirect HomeR
  where
    newOrganisationFor :: OrganisationId -> UTCTime -> User -> Organisation
    newOrganisationFor organisationId createdAt user =
      Organisation
        { organisationId
        , organisationName = newOrgNameFrom user
        , createdAt
        , updatedAt = createdAt
        }

    newOrgNameFrom :: User -> Text
    newOrgNameFrom User{..} = userOrgNamePrefix <> username

    userOrgNamePrefix :: Text
    userOrgNamePrefix = "default_org_for"
