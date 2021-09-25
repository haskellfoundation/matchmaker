module Handler.Account.Create where

import Control.Monad.Except (throwError)
import DB.Organisation
import DB.User
import Data.Time
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Transact (DBT)
import ImportYesod
import Model.UserModel
import Web.Form
import Web.Sessions

postAccountCreateR :: Handler ()
postAccountCreateR = do
  postParams <- getPostParams
  result <- runDB $ runExceptT $ postAccountCreate postParams
  case result of
    Left errors -> do
      putAssign "form_error" "true"
      handleFormErrors errors
      redirect SignupR
    Right _ ->
      redirect HomeR

postAccountCreate
  :: [(Text, Text)]
  -> ExceptT (NonEmpty NewUserValidationError) (DBT IO) ()
postAccountCreate postParams = do
  newUser <- liftIO $ validateNewUser postParams
  case newUser of
    FieldErrors errors -> throwError errors
    Result user@User{..} -> do
      orgId <- liftIO $ OrganisationId <$> nextRandom
      userOrgId <- liftIO $ UserOrganisationId <$> nextRandom
      timestamp <- liftIO getCurrentTime
      let org = newOrganisationFor orgId timestamp user
      lift $ insertUser user
        *> insertOrganisation org
        *> attachUser userId orgId userOrgId
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
    userOrgNamePrefix = "default_org_for_"
