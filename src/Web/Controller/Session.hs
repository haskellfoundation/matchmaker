module Web.Controller.Session
  ( new
  , create
  ) where

import Data.Password.Argon2 (Password, mkPassword)
import Web.Scotty.Trans

import Database.PostgreSQL.Entity.DBT
import DB.User
import Web.Auth
import Web.FlashAlerts
import Web.Templates.Partials.FlashAlerts
import Web.Types
import qualified Web.View.Session as SessionView

new :: ActionT MatchmakerError WebM ()
new = SessionView.login
      >>= html

create :: HasCallStack => ActionT MatchmakerError WebM ()
create = do
  email <- param "email"
  loginPassword <- mkPassword <$> param "password"
  pool <- asks pgPool
  result <- liftIO $ runDB pool $ getUserByEmail email
  case result of
    Just user -> validateLogin loginPassword user
    Nothing -> do
      putError $ errorTemplate "Login failure"
      redirect "/login"

validateLogin :: Password -> User -> ActionT MatchmakerError WebM ()
validateLogin loginPassword user = do
  if validatePassword loginPassword (password user)
  then do
    markUserAsAuthenticated (userId user)
    putInfo $ infoTemplate "Logged-in"
    redirect "/"
  else do
    putError $ errorTemplate "Login failure"
    redirect "/login"
