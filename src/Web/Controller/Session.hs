module Web.Controller.Session where

import Data.Password.Argon2 (mkPassword)
import Web.Scotty.Trans

import Database.PostgreSQL.Entity.DBT
import DB.User
import Web.Helpers
import Web.Types
import qualified Web.View.Home as HomeView

new :: ActionT LText WebM ()
new = do
  result <- HomeView.login
  html result

create :: ActionT LText WebM ()
create = do
  email <- param "login-email"
  loginPassword <- mkPassword <$> param "login-password"
  pool <- asks pgPool
  dbUser <- liftIO $ runDB pool $ getUserByEmail email
  case dbUser of
    Right user ->
      if validatePassword loginPassword (password user)
      then do
        redirect "/"
        markUserAsAuthenticated (userId user)
      else do
        redirect "/login"
    Left _ ->
      redirect "/login"
