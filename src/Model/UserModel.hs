{-# LANGUAGE StrictData #-}
module Model.UserModel where

import DB.User
import qualified Data.Map as M
import Data.Password.Argon2 (Password, mkPassword)
import qualified Data.Text as T
import Data.Time
import Data.UUID.V4 (nextRandom)
import Web.Form
import Web.Sessions

data NewUserValidationError
  = EmptyUsername
  | EmptyDisplayName
  | TooShortPassword
  | InvalidEmailAddress
  | MissingField Text
  deriving Show

instance ErrorToAssign NewUserValidationError where
  putErrorAssign err =
    case err of
      EmptyUsername       -> putAssign "form_error_username" "Username cannot be empty"
      EmptyDisplayName    -> putAssign "form_error_displayname" "Display name cannot be empty"
      TooShortPassword    -> putAssign "form_error_password" "Password cannot be smaller than 8 characters"
      InvalidEmailAddress -> putAssign "form_error_email" "Email address is invalid"
      MissingField field  -> putAssign ("form_error_" <> field) ("Missing required field: " <> field)

validateNewUser :: [(Text,Text)] -> IO (FormValidation NewUserValidationError User)
validateNewUser params = do
  let paramMap = M.fromList params
  ts <- getCurrentTime
  userId <- UserId <$> nextRandom
  hashedPassword <- traverse hashPassword $ validateShortPassword =<< lookupFormField MissingField "password" paramMap
  pure $
    User userId
    <$> (validateUsername =<< lookupFormField MissingField "username" paramMap)
    <*> (validateEmailAddress =<< lookupFormField MissingField "email" paramMap)
    <*> (validateDisplayName =<< lookupFormField MissingField "displayname" paramMap)
    <*> hashedPassword
    <*> pure ts
    <*> pure ts

validateUsername :: Text -> FormValidation NewUserValidationError Text
validateUsername name = if T.null name then fieldError EmptyUsername else pure name

validateDisplayName:: Text -> FormValidation NewUserValidationError Text
validateDisplayName name = if T.null name then fieldError EmptyDisplayName else pure name

validateShortPassword :: Text -> FormValidation NewUserValidationError Password
validateShortPassword password =
  if T.length password < 8
     then fieldError TooShortPassword
     else Result $ mkPassword password

validateEmailAddress :: Text -> FormValidation NewUserValidationError Text
validateEmailAddress email =
  if not . T.isInfixOf "@" $ email
     then fieldError InvalidEmailAddress
     else pure email
