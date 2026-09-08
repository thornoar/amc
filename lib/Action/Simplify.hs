{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Action.Simplify (SimplifyResult, simplifyResult) where

import Object.Bundle
import Result (Result (..))
import Data.Kind (Constraint)
import Description

import qualified Action.Instances.SimplifyIEX as SIEX
import qualified Action.Instances.SimplifyREX as SREX

type SimplifyResult :: ObjectTag -> Constraint
class SimplifyResult tg where
  simplifyResult :: Object tg -> Result (Object tg)

instance {-# OVERLAPPABLE #-} Description tg => SimplifyResult tg where
  simplifyResult obj = Error $ (description (proxyOf obj)) ++ " cannot be simplified"

instance {-# OVERLAPPING #-} SimplifyResult IEX where
  simplifyResult = Content . SIEX.simplify

instance {-# OVERLAPPING #-} SimplifyResult REX where
  simplifyResult = SREX.simplifyResult
