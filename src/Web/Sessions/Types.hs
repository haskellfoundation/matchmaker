module Web.Sessions.Types where

newtype UserAssigns = UserAssigns {getUserAssigns :: HashMap Text Text}
  deriving newtype (Show, Eq)
