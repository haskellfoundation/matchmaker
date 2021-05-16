module Web.Form.Types where

import Foundation (Handler)

data FormValidation e a = FieldErrors (NonEmpty e)
                        | Result a

instance Functor (FormValidation e) where
  fmap _ (FieldErrors fs) = FieldErrors fs
  fmap f (Result a)=      Result $ f a

instance Applicative (FormValidation e) where
  pure = Result
  (<*>) (FieldErrors fs) (FieldErrors gs) = FieldErrors $ fs <> gs
  (<*>) (FieldErrors fs) _                = FieldErrors fs
  (<*>) _ (FieldErrors fs)                = FieldErrors fs
  (<*>) (Result f) (Result a)             = Result $ f a

instance Monad (FormValidation e) where
  return = pure
  (>>=) (Result a) f      = f a
  (>>=) (FieldErrors f) _ = FieldErrors f

instance Bifunctor FormValidation where
  bimap f _ (FieldErrors e) = FieldErrors $ f <$> e
  bimap _ g (Result r)      = Result $ g r

instance Foldable (FormValidation e) where
  foldMap _ (FieldErrors _) = mempty
  foldMap f (Result r)      = f r

instance Traversable (FormValidation e) where
  sequenceA (FieldErrors e) = pure (FieldErrors e)
  sequenceA (Result fa)     = fmap Result fa

class ErrorToAssign e where
  putErrorAssign :: e -> Handler ()

handleFormErrors :: ErrorToAssign e => NonEmpty e -> Handler ()
handleFormErrors = mapM_ putErrorAssign
