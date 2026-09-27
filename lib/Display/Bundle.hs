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
import qualified Display.Instances.DisplayREX as DREX

type DisplayResult :: ObjectTag -> Constraint
class DisplayResult tg where
  displayResult :: Object tg -> Result String

instance {-# OVERLAPPABLE #-} Description tg => DisplayResult tg where
  displayResult obj = Error $ (description (proxyOf obj)) ++ " cannot be printed"

instance {-# OVERLAPPING #-} DisplayResult IEX where
  displayResult = Content . DIEX.display

instance {-# OVERLAPPING #-} DisplayResult REX where
  displayResult = Content . DREX.display

instance {-# OVERLAPPING #-} DisplayResult STR where
  displayResult (Raw str) = Content str
