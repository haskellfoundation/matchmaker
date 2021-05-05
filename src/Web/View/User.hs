module Web.View.User where

import Data.HashMap.Strict as HashMap
import Database.PostgreSQL.Entity.DBT
import Web.Scotty.Trans

import DB.User
import Web.Auth
import Web.Templates (render)
import Web.Templates.Helpers (moduleName)
import Web.Templates.Types
import Web.Types

show :: ActionT MatchmakerError WebM LText
show = do
  pool <- asks pgPool
  let template = TemplateName "show"
  result <- getUserIdFromSession
  assigns <- case result of
                  Nothing -> pure $ TemplateAssigns HashMap.empty
                  Just uId -> do
                    (Just User{..}) <- liftIO $ runDB pool $ getUserById uId
                    pure $ TemplateAssigns $ HashMap.fromList [("username", username)]
  render $$(moduleName) template assigns
