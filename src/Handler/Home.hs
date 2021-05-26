module Handler.Home where

import DB.User (User (..), UserId (..), getUserById)
import qualified Data.HashMap.Strict as HM
import Data.UUID (fromText)
import ImportYesod
import Templates (render)
import Templates.Helpers (moduleName)
import Templates.Types (TemplateAssigns (TemplateAssigns),
                        TemplateName (TemplateName))
import Web.Sessions (readAssign)

getHomeR :: Handler Html
getHomeR = do
  mUserId  <- readAssign "user_id" $ fmap UserId . fromText
  assigns <-
    maybe
      (pure $ TemplateAssigns HM.empty)
      (\uId -> do
          (Just user) <- runDB $ getUserById uId
          pure $ TemplateAssigns $ HM.fromList [("displayName", displayName user)]
      )
      mUserId
  render $$(moduleName) (TemplateName "index") assigns


