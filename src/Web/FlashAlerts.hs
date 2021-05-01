module Web.FlashAlerts where

import Web.Scotty.Trans (ActionT)

import Web.Sessions (getAssign, insertAssign, modifySession, readSession,
                     removeAssign)
import Web.Templates.Partials.FlashAlerts
import Web.Types (MatchmakerError, WebEnvironment (sessions), WebM)

putInfo :: Text -> ActionT MatchmakerError WebM ()
putInfo msg = putFlash "flash_alert_info" msg

putError :: Text -> ActionT MatchmakerError WebM ()
putError msg = putFlash "flash_alert_error" msg

getInfo :: ActionT MatchmakerError WebM (Maybe Text)
getInfo = popFlash "flash_alert_info"

getError :: ActionT MatchmakerError WebM (Maybe Text)
getError = popFlash "flash_alert_error"

putFlash :: Text -> Text -> ActionT MatchmakerError WebM ()
putFlash key value = do
  sm <- asks sessions
  modifySession sm (\mVal -> mVal >>= Just . insertAssign key value)

popFlash :: Text -> ActionT MatchmakerError WebM (Maybe Text)
popFlash flash = do
  sm <- asks sessions
  mUserAssigns <- readSession sm
  case mUserAssigns of
    Nothing -> pure Nothing
    Just ua -> do
      let content = getAssign flash ua
      modifySession sm (\mVal -> mVal >>= Just . removeAssign flash)
      pure content

getFlashes :: ActionT MatchmakerError WebM Text
getFlashes = do
  maybeError <- getError
  maybeInfo <- getInfo
  traceShowM maybeInfo
  let err = maybe "" errorTemplate maybeError
  let info    = maybe "" infoTemplate maybeInfo
  pure $ err <> info
