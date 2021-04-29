module Web.Controller.Account
  ( new
  , create
  ) where

import Web.Scotty.Trans
import Data.Time

import Model.UserModel
import Web.Helpers
import Web.Sessions (putAssign)
import Web.Types
import qualified Web.View.Account as AccountView
import DB.User
import Data.UUID.V4 (nextRandom)
import Data.Password.Argon2 (mkPassword)
import Database.PostgreSQL.Entity.DBT

new :: ActionT MatchmakerError WebM ()
new = AccountView.new
      >>= html

create :: HasCallStack => ActionT MatchmakerError WebM ()
create = do
  debug "Validating sign-up"
  usernameParam <- param "signup-username"
  emailParam <- param "signup-email"
  passwordParam <- param "signup-password"
  displayNameParam <- param "signup-displayname"
  pool <- asks pgPool
  let validationResult = validateNewUser NewUser{..}
  case validationResult of
    Left errors -> do
      putAssign "form_error" "true"
      mapM_ fillFieldErrors errors
      redirect "/signup"
    Right _ -> do
      ts <- liftIO getCurrentTime
      userId <- UserId <$> liftIO nextRandom
      let username = usernameParam
      let email = emailParam
      let displayName = displayNameParam
      password <- hashPassword (mkPassword passwordParam)
      let createdAt = ts
      let updatedAt = ts
      let user = User{..}
      liftIO $ runDB pool $ insertUser user
      redirect "/"

fillFieldErrors :: NewUserValidationError -> ActionT MatchmakerError WebM ()
fillFieldErrors e =
  case e of
    EmptyUsername -> putAssign "form_error_password" "Username cannot be empty"
    EmptyDisplayName -> putAssign "form_error_displayname" "Display name cannot be empty"
    TooShortPassword -> putAssign "form_error_password" "Password cannot be smaller than 8 characters"
    InvalidEmailAddress -> putAssign "form_error_email" "Email address is invalid"