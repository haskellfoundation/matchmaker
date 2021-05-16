{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module is important, it is where we import all of our handler functions and make
-- them available to Yesod. You should get a type error when you add a route to the router
-- and don't add a corresponding handler function here, but I figure this comment might be
-- useful since this is one of the more magical pieces of Yesod.
module Handler where

import Foundation
import Yesod.Core (mkYesodDispatch)

-- Handlers
import Handler.Account.Create
import Handler.Home
import Handler.Login
import Handler.Login.Signin
import Handler.Signup
import Handler.User

-- | This is the second half of the router template haskell. This is called the "dispatch" and
-- 'resourcesFoundation' is what takes all of our handler functions in scope and makes them available
-- to Yesod.
-- Note: unfortunately mkYesodDispatch introduces an instance for Handler, but Handler is
-- "defined" (by template haskell) in Foundation.hs. Therefore we have to turn off warning
-- on orphan instances. This file structure is nice, though, because it prevents import
-- cycles and allows us to import all our handler functions in one file.
mkYesodDispatch "Foundation" resourcesFoundation
