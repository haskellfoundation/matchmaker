module Web.View.Home where

import Data.HashMap.Strict as HashMap (fromList)

import Web.Scotty.Trans (ActionT)
import Web.Templates (render)
import Web.Templates.Helpers (moduleName)
import Web.Templates.Types (TemplateAssigns (TemplateAssigns),
                            TemplateName (TemplateName))
import Web.Types (WebM)

index :: ActionT LText WebM LText
index = render $$(moduleName) template context
  where
    template = TemplateName "index"

context :: TemplateAssigns
context = TemplateAssigns $ HashMap.fromList
    [ ("name", "Alice")
    , ("location", "Wonderland")
    ]
