module Web.View.Home where

import Data.HashMap.Strict as HashMap
import Database.PostgreSQL.Entity.DBT
import Web.Scotty.Trans (ActionT)

import DB.User (User (..), getUserById)
import Web.Auth
import Web.Templates (render)
import Web.Templates.Helpers (moduleName)
import Web.Templates.Types (TemplateAssigns (TemplateAssigns),
                            TemplateName (TemplateName))
import Web.Types (WebEnvironment (..), WebM, MatchmakerError)

index :: ActionT MatchmakerError WebM LText
index = do
  pool <- asks pgPool
  let template = TemplateName "index"
  result <- getUserIdFromSession
  assigns  <- case result of
                  Nothing ->
                    pure $ TemplateAssigns $ HashMap.empty
                  Just uId -> do
                    (Just user) <- liftIO $ runDB pool $ getUserById uId
                    pure $ TemplateAssigns $ HashMap.fromList [("displayName", displayName user)]
  render $$(moduleName) template assigns
