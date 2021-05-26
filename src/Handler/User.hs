module Handler.User where

import ImportYesod

import Data.HashMap.Strict as HashMap

import DB.User
import Templates (render)
import Templates.Helpers (moduleName)
import Templates.Types
import Web.Sessions

getUserR :: Handler Html
getUserR = do
  mUserId <- getUserIdFromSession
  assigns <- case mUserId of
                  Nothing -> pure $ TemplateAssigns HashMap.empty
                  Just uId -> do
                    (Just User{..}) <- runDB $ getUserById uId
                    pure $ TemplateAssigns $ HashMap.fromList [("username", username)]
  render $$(moduleName) (TemplateName "show") assigns
