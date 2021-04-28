module Web.Templates where

import qualified Data.HashMap.Strict as HashMap
import System.IO.Error (tryIOError)
import Text.Ginger (GVal, Source, SourceName, ToGVal (..), makeContextHtml,
                    parseGingerFile, runGinger)
import Text.Ginger.Html (htmlSource)
import Web.Scotty.Trans (ActionT)

import Web.Templates.Types (ModuleName (..), TemplateAssigns (getAssigns),
                            TemplateName (..))
import Web.Types (WebM, MatchmakerError)

render :: ModuleName -> TemplateName -> TemplateAssigns -> ActionT MatchmakerError WebM LText
render (ModuleName moduleName) (TemplateName templateName) assigns = do
  let templatePath = "./src/Web/Templates/" <> moduleName <> "/" <> templateName <> ".html"
  let hm = getAssigns assigns
  let contextLookup = flip scopeLookup hm
  let context = makeContextHtml contextLookup
  template' <- parseGingerFile resolver (toString templatePath)
  case template' of
    Left err -> pure $ show err
    Right template ->
      pure . toLText . htmlSource $ runGinger context template

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
