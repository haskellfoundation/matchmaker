{-# LANGUAGE StrictData #-}
module Model.UserModel where

import qualified Data.Text as T

import Validation
data NewUser =
  NewUser { usernameParam :: {-# UNPACK#-}Text
          , emailParam ::  {-# UNPACK#-}Text
          , passwordParam ::  {-# UNPACK#-}Text
          , displayNameParam :: {-# UNPACK#-}Text
          } deriving stock (Show, Eq)

data NewUserValidationError
  = EmptyUsername
  | EmptyDisplayName
  | TooShortPassword
  | InvalidEmailAddress

validateNewUser :: NewUser -> Either (NonEmpty NewUserValidationError) ()
validateNewUser NewUser{..} = validationToEither $
  validateEmptyUsername usernameParam
    <* validateEmptyDisplayName displayNameParam
    <* validateShortPassword passwordParam
    <* validateInvalidEmailAddress emailParam

validateEmptyUsername :: Text -> Validation (NonEmpty NewUserValidationError) ()
validateEmptyUsername name = failureIf (T.null name) EmptyUsername

validateEmptyDisplayName:: Text -> Validation (NonEmpty NewUserValidationError) ()
validateEmptyDisplayName name = failureIf (T.null name) EmptyDisplayName

validateShortPassword :: Text -> Validation (NonEmpty NewUserValidationError) ()
validateShortPassword password = failureIf (T.length password < 8) TooShortPassword

validateInvalidEmailAddress :: Text -> Validation (NonEmpty NewUserValidationError) ()
validateInvalidEmailAddress email = failureUnless (T.isInfixOf "@" email) InvalidEmailAddress
