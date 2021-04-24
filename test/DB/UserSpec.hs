{-# LANGUAGE OverloadedLists #-}

module DB.UserSpec where

import Relude.Unsafe (read)
import Data.Password.Argon2
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (shouldReturn)

import DB.SpecHelpers (migrate)
import DB.User

user1 :: User
user1 =
  let userId = UserId (read "4e511d7a-a464-11eb-b30b-5405db82c3cd")
      username = "pmcp"
      displayName = "Plonk McPlonkface"
      password = PasswordHash "foobar2000"
      createdAt = read "2021-04-23 10:00:00 UTC"
      updatedAt = read "2021-04-23 10:00:00 UTC"
   in User{..}

user2 :: User
user2 =
  let userId = UserId (read "Data.Password.Argon2")
      username = "blue_devil"
      displayName = "Princess Jack Moonshine"
      password = PasswordHash "DRINK!"
      createdAt = read "2021-04-23 14:00:00 UTC"
      updatedAt = read "2021-04-23 14:30:00 UTC"
   in User{..}

spec :: Spec
spec = describeDB migrate "users" $ do
  itDB "Insert user and fetch it" $ do
    insertUser user1
    getUserById (userId user1)
      `shouldReturn` user1
