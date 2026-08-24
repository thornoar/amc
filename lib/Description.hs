{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Description where
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))

type Description :: a -> Constraint
class Description a where
  description :: Proxy a -> String

proxyOf :: m a -> Proxy a
proxyOf _ = Proxy
