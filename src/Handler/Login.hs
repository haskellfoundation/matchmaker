module Handler.Login where

import ImportYesod
import Templates (render)
import Templates.Helpers (emptyAssigns, moduleName)
import Templates.Types (TemplateName (TemplateName))

getLoginR :: Handler Html
getLoginR =
  render $$(moduleName) (TemplateName "login") emptyAssigns


