module Web.Helpers where

import Colourista.IO (cyanMessage)
import qualified Data.HashMap.Strict as HM
import Data.Time (getCurrentTime)
import Web.Scotty.Trans (html)
import GHC.Stack (popCallStack)
import Web.Scotty.Internal.Types (ActionT, ScottyError (showError))

import Web.Templates (render)
import Web.Templates.Types (ModuleName (ModuleName),
                            TemplateAssigns (TemplateAssigns),
                            TemplateName (TemplateName))
import Web.Types (MatchmakerError (..), WebM)

errorHandler :: HasCallStack => MatchmakerError
             -> ActionT MatchmakerError WebM ()
errorHandler err = do
  let assigns = TemplateAssigns $ HM.fromList [("error", toStrict $ showError err), ("stacktrace", toText $ prettyCallStack callStack)]
  html =<< render (ModuleName "Error") (TemplateName "500") assigns

debug :: HasCallStack => (MonadIO m) => Text -> m ()
debug msg = do
  ts <- liftIO getCurrentTime
  liftIO $ cyanMessage $ show ts <> " [Debug] " <> msg
  liftIO $ cyanMessage $ toText $ prettyCallStack $ popCallStack  callStack
