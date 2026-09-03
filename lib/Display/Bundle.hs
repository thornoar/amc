{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Display.Bundle (DisplayResult, displayResult) where

import Object.Bundle
import Data.Kind (Constraint)
-- import Description
import Result
import Description

import qualified Display.Instances.DisplayIEX as DIEX

type DisplayResult :: ObjectTag -> Constraint
class DisplayResult tg where
  displayResult :: Object tg -> Result String
  -- default displayResult :: Display tg => Object tg -> Result String
  -- displayResult = Content . display
instance {-# OVERLAPPABLE #-} Description tg => DisplayResult tg where
  displayResult obj = Error $ (description (proxyOf obj)) ++ " cannot be printed"

-- type Display :: ObjectTag -> Constraint
-- class Display tg where
--   display :: Object tg -> String
--   -- default print :: Show (Object tg) => Object tg -> String
--   -- print = show

instance {-# OVERLAPPING #-} DisplayResult IEX where
  displayResult = Content . DIEX.display
-- instance Display IEX where
--   display = DIEX.display

instance {-# OVERLAPPING #-} DisplayResult REX where
  displayResult = Content . show
-- instance Display REX where
--   display = show

instance {-# OVERLAPPING #-} DisplayResult STR where
  displayResult (Raw str) = Content str
