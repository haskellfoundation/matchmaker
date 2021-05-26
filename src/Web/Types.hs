{-# LANGUAGE StrictData #-}
module Web.Types
  ( DBError(..)
  , MatchmakerError(..)
  , WebError(..)
  ) where

data MatchmakerError
  = DB DBError
  | Web WebError
  | TextError {-# UNPACK #-}Text
  deriving stock (Eq, Generic, Show)

data WebError
  = LoginFailure
  deriving stock (Eq, Generic, Show)

instance Exception WebError

data DBError
  = ConstraintError {-# UNPACK #-}Text
  | NotFound
  | TooManyResults
  | InsertionError
  | DeserialisationError {-# UNPACK #-}Text
  deriving stock (Eq, Generic, Show)

instance Exception DBError
