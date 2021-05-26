module Web.Form (
  module Web.Form,
  module Web.Form.Types,
) where

import Data.Map hiding (fold, toList)
import ImportYesod
import Templates.Partials.FlashAlerts
import Web.FlashAlerts
import Web.Form.Types

fieldError :: e -> FormValidation e a
fieldError err = FieldErrors $ err :| []

lookupFormFieldTextError :: Text -> Map Text Text -> FormValidation Text Text
lookupFormFieldTextError k m = lookupFormField id k m

lookupFormField :: (Text -> e) -> Text -> Map Text Text -> FormValidation e Text
lookupFormField err k m =
  case lookup k m of
    Nothing -> fieldError . err $ k
    Just v  -> Result v

lookupOptionalFormField :: Text -> Map Text Text -> FormValidation Text (Maybe Text)
lookupOptionalFormField k m = Result $ lookup k m

handleMissingFields :: Route Foundation -> FormValidation Text a -> Handler a
handleMissingFields route (FieldErrors fields) = do
  putError . errorTemplate . fold . intersperse ", " $ toList fields
  redirect route
handleMissingFields _ (Result res) = pure res
