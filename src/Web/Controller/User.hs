module Web.Controller.User where

import Web.Scotty.Trans

import Web.Types
import qualified Web.View.User as UserView

show :: ActionT MatchmakerError WebM ()
show = UserView.show
       >>= html
