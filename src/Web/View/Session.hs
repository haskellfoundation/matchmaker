module Web.View.Session where

import Web.Scotty.Trans

import Web.Types
import Web.Templates.Helpers (moduleName, emptyAssigns)
import Web.Templates
import Web.Templates.Types

login :: ActionT MatchmakerError WebM LText
login =
  render $$(moduleName) (TemplateName "login") emptyAssigns
