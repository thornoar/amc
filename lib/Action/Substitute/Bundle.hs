{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Action.Substitute.Bundle (SubstituteResult, substituteResult) where

import Object.Bundle
import Data.Kind (Constraint)
import Input (IIO)
import Description
import Result

import qualified Action.Substitute.Instances.IEX as SubIEX
import qualified Action.Substitute.Instances.REX as SubREX

type SubstituteResult :: ObjectTag -> Constraint
class SubstituteResult tg where
  substituteResult :: Object tg -> IIO (Result (Object tg))

instance {-# OVERLAPPABLE #-} Description tg => SubstituteResult tg where
  substituteResult obj = return . Error $ (description (proxyOf obj)) ++ " cannot be used for substitution"

instance {-# OVERLAPPING #-} SubstituteResult IEX where
  substituteResult = SubIEX.substituteResult

instance {-# OVERLAPPING #-} SubstituteResult REX where
  substituteResult = SubREX.substituteResult
