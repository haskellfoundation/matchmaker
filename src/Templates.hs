module Templates where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HashMap
import Foundation
import System.IO.Error (tryIOError)
import Templates.Types (ModuleName (..), TemplateAssigns (..),
                        TemplateName (..))
import Text.Ginger (GVal, Source, SourceName, ToGVal (..), makeContextHtml,
                    parseGingerFile, runGinger)
import Text.Ginger.Html (htmlSource)
import Web.Helpers (debug)
import Web.Sessions (UserAssigns (UserAssigns), getAllUserAssigns, popAssign)
import Web.Types (MatchmakerError)
import Yesod.Core

render :: ModuleName -> TemplateName -> TemplateAssigns -> Handler Html
render (ModuleName moduleName) (TemplateName templateName) assigns = do
  let templatePath = "./src/Templates/" <> moduleName <> "/" <> templateName <> ".html"
  mUserAssigns <- getAllUserAssigns
  let (TemplateAssigns hm) = mkAssigns assigns mUserAssigns
  debug ("Assigns: " <> show hm)
  let contextLookup = flip scopeLookup hm
  let context = makeContextHtml contextLookup
  eTemplate <- liftIO $ parseGingerFile resolver (toString templatePath)
  case eTemplate of
    Left err -> pure $ show err
    Right template -> do
      popAssign "flash_alert_info"
      popAssign "flash_alert_error"
      pure . preEscapedToMarkup . htmlSource $ runGinger context template

mkAssigns :: TemplateAssigns -> Maybe UserAssigns -> TemplateAssigns
mkAssigns (TemplateAssigns templateAssigns) (Just (UserAssigns userAssigns)) =
  TemplateAssigns $ HM.union templateAssigns userAssigns
mkAssigns ta Nothing = ta

resolver :: SourceName -> IO (Maybe Source)
resolver templatePath = do
  e <- liftIO $ tryIOError $ readFile templatePath
  case e of
    Right contents -> pure (Just contents)
    Left _         -> pure Nothing

-- Wrapper around HashMap.lookup that applies toGVal to the value found.
-- Any value referenced in a template, returned from within a template, or used
-- in a template context, will be a GVal
scopeLookup ::
  (Hashable k, Eq k, ToGVal m b) =>
  k ->
  HashMap.HashMap k b ->
  GVal m
scopeLookup key context = toGVal $ HashMap.lookup key context

errorHandler ::
  HasCallStack =>
  MatchmakerError ->
  Handler Html
errorHandler err = do
  let assigns = TemplateAssigns $ HM.fromList [("error", toStrict $ show err), ("stacktrace", toText $ prettyCallStack callStack)]
  render (ModuleName "Error") (TemplateName "500") assigns
