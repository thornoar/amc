{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Result where
import Description
import Text.Read (readMaybe)

data Result a = Content a | Error String deriving Functor

deriving instance Show a => Show (Result a)

instance Applicative Result where
  pure = Content
  mf <*> ma = mf >>= flip fmap ma

instance Monad Result where
  ma >>= f = case ma of
    Error msg -> Error msg
    Content a -> f a

switch :: (String -> b) -> (a -> b) -> Result a -> b
switch f g ma = case ma of
  Error msg -> f msg
  Content a -> g a

readResult :: (Read a, Description a) => String -> Result a
readResult str = let ma = readMaybe str in case ma of
  Nothing -> Error $ "cannot read `" ++ str ++ "` as " ++ description (proxyOf ma)
  Just a -> Content a

todo :: a
todo = error "Not implemented yet"
