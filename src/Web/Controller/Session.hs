module Web.Controller.Session
  ( new
  , create
  ) where

import Data.Password.Argon2 (Password, mkPassword)
import Web.Scotty.Trans

import DB.User
import Web.FlashAlerts
import Web.Helpers
import Web.Types
import qualified Web.View.Session as SessionView
import Web.Auth
import Web.Templates.Partials.FlashAlerts
import Database.PostgreSQL.Entity.DBT

new :: ActionT MatchmakerError WebM ()
new = SessionView.login
      >>= html

create :: HasCallStack => ActionT MatchmakerError WebM ()
create = do
  email <- param "login-email"
  loginPassword <- mkPassword <$> param "login-password"
  pool <- asks pgPool
  debug "Validating login"
  result <- liftIO $ runDB pool $ getUserByEmail email
  case result of
    Just user -> validateLogin loginPassword user
    Nothing -> do
      putError $ errorTemplate "Login failure"
      redirect "/login"

validateLogin :: HasCallStack => Password -> User -> ActionT MatchmakerError WebM ()
validateLogin loginPassword user = do
  if validatePassword loginPassword (password user)
  then do
    debug "User is authenticated"
    markUserAsAuthenticated (userId user)
    putInfo $ infoTemplate "Logged-in"
    redirect "/"
  else do
    putError $ errorTemplate "Login failure"
    debug "Passwords do not match!"
    redirect "/login"
