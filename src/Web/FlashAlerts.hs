module Web.FlashAlerts where

import Web.Scotty.Trans (ActionT)

import Web.Sessions (popAssign, putAssign)
import Web.Templates.Partials.FlashAlerts (errorTemplate, infoTemplate)
import Web.Types (MatchmakerError, WebM)

getFlashes :: ActionT MatchmakerError WebM Text
getFlashes = do
  maybeError <- getError
  maybeInfo <- getInfo
  traceShowM maybeInfo
  let err = maybe "" errorTemplate maybeError
  let info    = maybe "" infoTemplate maybeInfo
  pure $ err <> info

putInfo :: Text -> ActionT MatchmakerError WebM ()
putInfo msg = putAssign "flash_alert_info" msg

putError :: Text -> ActionT MatchmakerError WebM ()
putError msg = putAssign "flash_alert_error" msg

getInfo :: ActionT MatchmakerError WebM (Maybe Text)
getInfo = popAssign "flash_alert_info"

getError :: ActionT MatchmakerError WebM (Maybe Text)
getError = popAssign "flash_alert_error"
