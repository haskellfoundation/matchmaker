module Web.View.Session where


import Web.Scotty.Trans
import qualified Data.HashMap.Strict as HashMap

import Web.Types
import Web.Templates.Helpers (moduleName)
import Web.Templates.Types
import Web.Templates
import Web.Sessions
import Web.Helpers

login :: ActionT MatchmakerError WebM LText
login = do
  fetchedSession <- readSession =<< asks sessions
  let assigns = case fetchedSession of
                  Just (UserAssigns hm) -> hm
                  Nothing -> HashMap.empty
  debug ("Assigns: " <> show assigns)
  render $$(moduleName) (TemplateName "login") (TemplateAssigns assigns)
