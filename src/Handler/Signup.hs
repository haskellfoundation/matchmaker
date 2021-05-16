module Handler.Signup where

import ImportYesod
import Templates (render)
import Templates.Helpers (emptyAssigns, moduleName)
import Templates.Types (TemplateName (TemplateName))

getSignupR :: Handler Html
getSignupR = render $$(moduleName) (TemplateName "signup") emptyAssigns
