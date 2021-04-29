module Web.View.Session where

import Web.Scotty.Trans

import Web.Templates
import Web.Templates.Helpers (emptyAssigns, moduleName)
import Web.Templates.Types
import Web.Types

login :: ActionT MatchmakerError WebM LText
login =
  render $$(moduleName) (TemplateName "login") emptyAssigns
