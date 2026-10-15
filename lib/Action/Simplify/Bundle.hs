{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Action.Simplify.Bundle (SimplifyResult, simplifyResult) where

import Object.Bundle
import Result (Result (..))
import Data.Kind (Constraint)
import Description

import qualified Action.Simplify.Instances.IEX as SIEX
import qualified Action.Simplify.Instances.REX as SREX
import Input (IIO)

type SimplifyResult :: ObjectTag -> Constraint
class SimplifyResult tg where
  simplifyResult :: Object tg -> IIO (Result (Object tg))

instance {-# OVERLAPPABLE #-} Description tg => SimplifyResult tg where
  simplifyResult obj = return . Error $ (description (proxyOf obj)) ++ " cannot be simplified"

instance {-# OVERLAPPING #-} SimplifyResult IEX where
  simplifyResult = return . Content . SIEX.simplify

instance {-# OVERLAPPING #-} SimplifyResult REX where
  simplifyResult = return . SREX.simplifyResult
