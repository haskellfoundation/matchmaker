{-# LANGUAGE LambdaCase #-}

module Web.AccountCreationSpec where

import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (expectationFailure)

import DB.Organisation (getOrganisationByName)
import DB.SpecHelpers (migrate)
import Handler.Account.Create (postAccountCreate)

postData1 :: [(Text, Text)]
postData1 =
  [ ("username"   , "wildcat")
  , ("email"      , "force_captain@horde.io")
  , ("displayname", "Catra")
  , ("password"   , "adorauwu")
  ]

spec :: Spec
spec = describeDB migrate "org" $ do
  itDB "Users have a default organisation" $ do
    let orgName = "default_org_for_wildcat"
    runExceptT (postAccountCreate postData1) >>= \case
      Left errors -> expectationFailure $ "Validation error(s): " <> show errors
      Right _     -> pure ()
    whenNothingM_ (getOrganisationByName orgName)
      $ expectationFailure "no default org created or name formatting changed"
