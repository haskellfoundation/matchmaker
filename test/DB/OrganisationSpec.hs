
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module DB.OrganisationSpec where

import Data.Password.Argon2
import Data.UUID.V4
import Relude.Unsafe (read)
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (expectationFailure, shouldReturn)

import DB.Organisation (Organisation (..), OrganisationId (..),
                        UserOrganisationId (..), attachUser, getAdmins,
                        getAllUserOrganisations, getOrganisationByName,
                        getUserOrganisation, getUserOrganisationById, getUsers,
                        insertOrganisation, makeAdmin)
import DB.SpecHelpers (migrate)
import DB.User

user1 :: User
user1 =
  let userId = UserId (read "4e511d7a-a464-11eb-b30b-5405db82c3cd")
      username = "pmpc"
      email = "pmpc@example.com"
      displayName = "Plonk McPlonkface"
      password = PasswordHash "foobar2000"
      createdAt = read "2021-04-23 10:00:00 UTC"
      updatedAt = read "2021-04-23 10:00:00 UTC"
   in User{..}

user2 :: User
user2 =
  let userId = UserId (read "44495a98-a475-11eb-94f3-5405db82c3cd")
      username = "blue_devil"
      email = "princess_jack@example.com"
      displayName = "Princess Jack Moonshine"
      password = PasswordHash "DRINK!"
      createdAt = read "2021-04-23 14:00:00 UTC"
      updatedAt = read "2021-04-23 14:30:00 UTC"
   in User{..}

organisation1 :: Organisation
organisation1 =
  let organisationId = OrganisationId (read "6e9b2ff8-a469-11eb-b05c-5405db82c3cd")
      organisationName = "haskell-servant"
      createdAt = read "2021-03-30 01:00:00 UTC"
      updatedAt = read "2021-03-30 01:00:00 UTC"
   in Organisation{..}

organisation2 :: Organisation
organisation2 =
  let organisationId = OrganisationId (read "b63ad088-a474-11eb-9236-5405db82c3cd")
      organisationName = "ghchq"
      createdAt = read "2021-04-10 01:00:00 UTC"
      updatedAt = read "2021-04-11 01:00:00 UTC"
   in Organisation{..}

spec :: Spec
spec = describeDB migrate "users" $ do
  itDB "Attach user1 to organisation1" $ do
    let uid = userId user1
    let oid = organisationId organisation1
    let uoId = UserOrganisationId (read "e801f560-a4dd-11eb-844b-5405db82c3cd")
    insertOrganisation organisation1
    insertUser user1
    attachUser uid oid uoId
    uos <- getUserOrganisationById uoId
    getUserOrganisation uid oid
      `shouldReturn` uos
  itDB "Look for admins in the organisation" $ do
    let uid = userId user2
    let oid = organisationId organisation2
    let uoId = UserOrganisationId (read "f865652c-a4dd-11eb-8a43-5405db82c3cd")
    insertOrganisation organisation2
    insertUser user2
    attachUser uid oid uoId
    makeAdmin uid oid
    uo <- getUserOrganisation uid oid
    print uo
    getAdmins (organisationId organisation2)
      `shouldReturn` [user2]
