module Web.Templates where

import qualified Data.HashMap.Strict as HashMap
import System.IO.Error (tryIOError)
import Text.Ginger (GVal, Source, SourceName, ToGVal (..), makeContextHtml,
                    parseGingerFile, runGinger)
import Text.Ginger.Html (htmlSource)
import Web.Scotty.Trans (ActionT, html, showError)

import qualified Data.HashMap.Strict as HM
import Web.FlashAlerts (popFlash)
import Web.Helpers (debug)
import Web.Sessions (UserAssigns (UserAssigns), UserAssigns (UserAssigns),
                     getSession, popAssign, readSession)
import Web.Templates.Types (ModuleName (..), TemplateAssigns (..),
                            TemplateName (..))
import Web.Types (MatchmakerError, WebM, sessions)

render :: ModuleName -> TemplateName -> TemplateAssigns -> ActionT MatchmakerError WebM LText
render (ModuleName moduleName) (TemplateName templateName) assigns = do
  let templatePath = "./src/Web/Templates/" <> moduleName <> "/" <> templateName <> ".html"
  (TemplateAssigns hm) <- mkAssigns assigns
  debug ("Assigns: " <> show hm)
  let contextLookup = flip scopeLookup hm
  let context = makeContextHtml contextLookup
  template' <- parseGingerFile resolver (toString templatePath)
  case template' of
    Left err -> pure $ show err
    Right template -> do
      popAssign "flash_alert_info"
      popAssign "flash_alert_error"
      pure . toLText . htmlSource $ runGinger context template

mkAssigns :: TemplateAssigns -> ActionT MatchmakerError WebM TemplateAssigns
mkAssigns (TemplateAssigns templateAssigns) = do
  fetchedSession <- readSession =<< asks sessions
  let userAssigns = case fetchedSession of
                  Just (UserAssigns hm) -> hm
                  Nothing               -> HashMap.empty
  pure $ TemplateAssigns $ HM.union templateAssigns userAssigns

resolver :: SourceName -> ActionT MatchmakerError WebM (Maybe Source)
resolver templatePath = do
  e <- liftIO $ tryIOError $ readFile templatePath
  case e of
    Right contents -> pure (Just contents)
    Left _         -> pure Nothing

-- Wrapper around HashMap.lookup that applies toGVal to the value found.
-- Any value referenced in a template, returned from within a template, or used
-- in a template context, will be a GVal
scopeLookup :: (Hashable k, Eq k, ToGVal m b)
            => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context = toGVal $ HashMap.lookup key context

errorHandler :: HasCallStack => MatchmakerError
             -> ActionT MatchmakerError WebM ()
errorHandler err = do
  let assigns = TemplateAssigns $ HM.fromList [("error", toStrict $ showError err), ("stacktrace", toText $ prettyCallStack callStack)]
  html =<< render (ModuleName "Error") (TemplateName "500") assigns
