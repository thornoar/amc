module Action.Differentiate.Bundle (DifferentiateResult, differentiateResult) where

import Object.Bundle
import Data.Kind (Constraint)
import Result
import Description

import qualified Action.Differentiate.Instances.IEX as DIEX
import qualified Action.Differentiate.Instances.REX as DREX
import System.Console.Haskeline (outputStrLn)
import Input (getColoredInputLine, promptLength, IIO)

type DifferentiateResult :: ObjectTag -> Constraint
class DifferentiateResult tg where
  differentiateResult :: Object tg -> IIO (Result (Object tg))

diffWith ::
  (String -> Object tg -> Result (Object tg)) ->
  Object tg ->
  IIO (Result (Object tg))
diffWith f obj = do
  outputStrLn "specify the list of variables for differentiation, separated by space:"
  let prstr = " variables: "
  input <- getColoredInputLine $ replicate (promptLength - length prstr) '#' ++ prstr
  return $ (fmap words input) >>= \vars -> foldl (\mo -> (mo >>=) . f) (Content obj) vars

instance {-# OVERLAPPABLE #-} Description tg => DifferentiateResult tg where
  differentiateResult obj = return . Error $ description (proxyOf obj) ++ " cannot be differentiated"

instance {-# OVERLAPPING #-} DifferentiateResult IEX where
  differentiateResult = diffWith DIEX.diff

instance {-# OVERLAPPING #-} DifferentiateResult REX where
  differentiateResult = diffWith DREX.diff
