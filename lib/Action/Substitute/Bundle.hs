{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Action.Substitute.Bundle (substituteResult) where

import Object.Bundle
import Data.Kind (Constraint)
import Input (IIO)
import Description
import Result

type SubstituteResult :: ObjectTag -> Constraint
class SubstituteResult tg where
  substituteResult :: Object tg -> IIO (Result (Object tg))

instance {-# OVERLAPPABLE #-} Description tg => SubstituteResult tg where
  substituteResult obj = return . Error $ (description (proxyOf obj)) ++ " cannot be used for substitution"
