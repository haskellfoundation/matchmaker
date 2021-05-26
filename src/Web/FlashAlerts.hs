module Web.FlashAlerts where

import ImportYesod
import Templates.Partials.FlashAlerts (errorTemplate, infoTemplate)
import Web.Sessions (popAssign, putAssign)

getFlashes :: Handler Text
getFlashes = do
  maybeError <- getError
  maybeInfo <- getInfo
  traceShowM maybeInfo
  let err = maybe "" errorTemplate maybeError
  let info    = maybe "" infoTemplate maybeInfo
  pure $ err <> info

putInfo :: Text -> Handler ()
putInfo msg = putAssign "flash_alert_info" msg

putError :: Text -> Handler ()
putError msg = putAssign "flash_alert_error" msg

getInfo :: Handler (Maybe Text)
getInfo = popAssign "flash_alert_info"

getError :: Handler (Maybe Text)
getError = popAssign "flash_alert_error"
