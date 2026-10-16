module Action.Substitute.Instances.REX (substituteResult) where

import Control.Monad (foldM, unless)
import Data.Char (isSpace)
import Data.List (nub)

import Input (IIO, getColoredInputLine, promptLength)
import Object.Bundle
import Parse.Instances.REX (parse)
import Result
import System.Console.Haskeline (outputStr)

substituteResult :: Object REX -> IIO (Result (Object REX))
substituteResult obj = do
  let vars = nub (variables obj)
      step (Error msg) _ = return (Error msg)
      step (Content e) name = do
        let prstr = " " ++ name ++ " = "
        minput <- getColoredInputLine $ replicate (promptLength - length prstr) '#' ++ prstr
        case minput of
          Error msg -> return (Error msg)
          Content str
            | all isSpace str -> return (Content e)
            | otherwise -> return (replace e name <$> parse str)
  res <- foldM step (Content obj) vars
  unless (null vars) (outputStr "\n")
  return res

variables :: Object REX -> [String]
variables (RConst _) = []
variables (RVar name) = [name]
variables (RNeg e) = variables e
variables (RApp _ e) = variables e
variables (RSum e1 e2) = variables e1 ++ variables e2
variables (RInv e) = variables e
variables (RProd e1 e2) = variables e1 ++ variables e2
variables (RDiv e1 e2) = variables e1 ++ variables e2
variables (RLog e1 e2) = variables e1 ++ variables e2
variables (RPow e1 e2) = variables e1 ++ variables e2

replace :: Object REX -> String -> Object REX -> Object REX
replace (RConst c) _ _ = RConst c
replace (RVar n) name value
  | n == name = value
  | otherwise = RVar n
replace (RNeg e) name value = RNeg (replace e name value)
replace (RApp f e) name value = RApp f (replace e name value)
replace (RSum e1 e2) name value = RSum (replace e1 name value) (replace e2 name value)
replace (RInv e) name value = RInv (replace e name value)
replace (RProd e1 e2) name value = RProd (replace e1 name value) (replace e2 name value)
replace (RDiv e1 e2) name value = RDiv (replace e1 name value) (replace e2 name value)
replace (RLog e1 e2) name value = RLog (replace e1 name value) (replace e2 name value)
replace (RPow e1 e2) name value = RPow (replace e1 name value) (replace e2 name value)
