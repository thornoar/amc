{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Description where
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))

type Description :: a -> Constraint
class Description a where
  description :: Proxy a -> String

proxyOf :: m a -> Proxy a
proxyOf _ = Proxy

proxyOf2 :: m1 (m2 a) -> Proxy a
proxyOf2 _ = Proxy

instance {-# OVERLAPPING #-} Description Double where description _ = "a floating-point number"
instance {-# OVERLAPPABLE #-} Integral a => Description a where description _ = "an integer"
