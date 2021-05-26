{-# OPTIONS_GHC -Wno-unused-imports #-}
module Web.Router (router) where

import Prelude hiding (get)
import Rowdy.Yesod
import Yesod.Core
import Yesod.Routes.TH.Types (ResourceTree)


router :: [ResourceTree String]
router = toYesod $ do
  get "HomeR"
  "login" // do
     get "LoginR"
     "signin" // post "LoginSigninR"

  "signup" // get "SignupR"
  "account" // "create" // post "AccountCreateR"
  "user" // get "UserR"
