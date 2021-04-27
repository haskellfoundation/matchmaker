module Web.View.Home where

import Data.HashMap.Strict as HashMap
import Database.PostgreSQL.Entity.DBT
import Web.Scotty.Trans (ActionT)

import DB.User (User (..), getUserById)
import Web.Helpers
import Web.Templates (render)
import Web.Templates.Helpers (moduleName)
import Web.Templates.Types (TemplateAssigns (TemplateAssigns),
                            TemplateName (TemplateName))
import Web.Types (WebEnvironment (..), WebM)

index :: ActionT LText WebM LText
index = do
  pool <- asks pgPool
  let template = TemplateName "index"
  isAuthed <- isUserAuthenticated
  assigns  <- if isAuthed
              then do
                result <- getUserId
                case result of
                  Nothing -> undefined
                  Just uId -> do
                    dbUser <- liftIO $ runDB pool $ getUserById uId
                    case dbUser of
                      Right user ->
                        pure $ TemplateAssigns $ HashMap.fromList [("displayName", displayName user)]
                      Left _ -> pure $ TemplateAssigns HashMap.empty
              else pure $ TemplateAssigns HashMap.empty
  render $$(moduleName) template assigns

login :: ActionT LText WebM LText
login = render $$(moduleName) template (TemplateAssigns HashMap.empty)
  where
    template = TemplateName "login"
