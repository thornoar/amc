{-# LANGUAGE DeriveFunctor #-}
module Result where

data Result a = Content a | Error String deriving Functor

instance Applicative Result where
  pure = Content
  mf <*> ma = mf >>= flip fmap ma

instance Monad Result where
  ma >>= f = case ma of
    Error msg -> Error msg
    Content a -> f a
