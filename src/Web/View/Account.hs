module Web.View.Account where

import Web.Scotty.Trans

import Web.Types
import Web.Templates.Helpers
import Web.Templates.Types
import Web.Templates

new :: ActionT MatchmakerError WebM LText
new = do
  render $$(moduleName) (TemplateName "signup") emptyAssigns
