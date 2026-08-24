{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Action where

-- import Simplify
-- import Parse
-- import Data.Kind (Constraint)
-- import Object

data ActionTag = SIMPL

-- action :: (Actionable tg tp1 tp2) => tg -> tp1 -> tp2
-- action SIMPL = simplify
