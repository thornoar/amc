{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Action.Display.Bundle (DisplayResult, displayResult) where

import Object.Bundle
import Data.Kind (Constraint)
-- import Description
import Result
import Description

import qualified Action.Display.Instances.IEX as DIEX
import qualified Action.Display.Instances.REX as DREX
import Input (IIO)
import System.Console.Haskeline (outputStrLn)

returnAfter :: (a -> IIO b) -> (a -> c) -> a -> IIO c
returnAfter act transf a = act a >> return (transf a)

type DisplayResult :: ObjectTag -> Constraint
class DisplayResult tg where
  displayResult :: Object tg -> IIO (Result (Object tg))

instance {-# OVERLAPPABLE #-} Description tg => DisplayResult tg where
  displayResult obj = return . Error $ (description (proxyOf obj)) ++ " cannot be printed"

instance {-# OVERLAPPING #-} DisplayResult IEX where
  displayResult = returnAfter (outputStrLn . DIEX.display) Content

instance {-# OVERLAPPING #-} DisplayResult REX where
  displayResult = returnAfter (outputStrLn . DREX.display) Content

instance {-# OVERLAPPING #-} DisplayResult STR where
  displayResult (Raw str) = returnAfter outputStrLn (Content . Raw) str
