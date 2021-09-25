module Main where

import Test.Hspec

import qualified DB.OrganisationSpec as OrganisationSpec
import qualified DB.UserSpec as UserSpec
import qualified Web.AccountCreationSpec as AccountCreationSpec
-- import qualified RepositorySpec as RepositorySpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    UserSpec.spec
    OrganisationSpec.spec
    AccountCreationSpec.spec
