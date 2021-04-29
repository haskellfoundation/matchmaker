module Web.Helpers where

import Colourista.IO (cyanMessage)
import Data.Time (getCurrentTime)
import GHC.Stack (popCallStack)

debug :: HasCallStack => (MonadIO m) => Text -> m ()
debug msg = do
  ts <- liftIO getCurrentTime
  liftIO $ cyanMessage $ show ts <> " [Debug] " <> msg
  liftIO $ cyanMessage $ toText $ prettyCallStack $ popCallStack  callStack
