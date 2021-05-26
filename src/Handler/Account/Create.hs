module Handler.Account.Create where

import DB.User
import ImportYesod
import Model.UserModel
import Web.Form
import Web.Sessions

postAccountCreateR :: Handler ()
postAccountCreateR = do
  postParams <- getPostParams
  newUser <- liftIO $ validateNewUser postParams
  case newUser of
    FieldErrors errors -> do
      putAssign "form_error" "true"
      handleFormErrors errors
      redirect SignupR
    Result user -> do
      runDB $ insertUser user
      redirect HomeR
