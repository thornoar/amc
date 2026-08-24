{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Action where

import Simplify
import Data.Kind (Constraint)
import Object

data ActionTag = SIMPL | PARSE

type Actionable :: ActionTag -> ObjectTag -> ObjectTag -> Constraint
type family Actionable at ot1 ot2 where
  Actionable SIMPL t t = Simplify t

-- action :: (Actionable tg tp1 tp2) => tg -> tp1 -> tp2
-- action SIMPL = simplify
