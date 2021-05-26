module Handler.Login.Signin where

import Data.Password.Argon2

import DB.User
import qualified Data.Map as M
import ImportYesod
import Templates.Partials.FlashAlerts
import Web.FlashAlerts
import Web.Form
import Web.Sessions (markAuthenticated)

postLoginSigninR :: Handler ()
postLoginSigninR = do
  postParams <- getPostParams
  SigninForm{..} <- handleMissingFields LoginSigninR $ parseSigninFormParams postParams
  mUser <- runDB $ getUserByEmail signinFormEmail
  case mUser of
    Just user -> validateLogin signinFormPassword user
    Nothing -> do
      putError $ errorTemplate "Login failure"
      redirect LoginR

data SigninForm =
  SigninForm
    { signinFormEmail    :: Text
    , signinFormPassword :: Password
    }

parseSigninFormParams :: [(Text, Text)] -> FormValidation Text SigninForm
parseSigninFormParams params =
  let paramMap = M.fromList params
      mPassword = mkPassword <$> lookupFormFieldTextError "password" paramMap
      mEmail = lookupFormFieldTextError "email" paramMap
   in liftA2 SigninForm mEmail mPassword

validateLogin :: Password -> User -> Handler ()
validateLogin loginPassword user = do
  if validatePassword loginPassword (password user)
  then do
    markAuthenticated (userId user)
    putInfo $ infoTemplate "Logged-in"
    redirect HomeR
  else do
    putError $ errorTemplate "Login failure"
    redirect LoginR
