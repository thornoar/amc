module Action.Substitute.Instances.IEX (substituteResult) where

import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.List (nub)

import Input (IIO, getColoredInputLine, promptLength)
import Object.Bundle
import Parse.Instances.IEX (parse)
import Result

substituteResult :: Object IEX -> IIO (Result (Object IEX))
substituteResult obj = foldM step (Content obj) (nub (variables obj))
  where
    step (Error msg) _ = return (Error msg)
    step (Content e) name = do
      let prstr = " " ++ name ++ " = "
      minput <- getColoredInputLine $ replicate (promptLength - length prstr) '#' ++ prstr
      case minput of
        Error msg -> return (Error msg)
        Content str
          | all isSpace str -> return (Content e)
          | otherwise -> return (replace e name <$> parse str)

variables :: Object IEX -> [String]
variables (IConst _) = []
variables (IVar name) = [name]
variables (INeg e) = variables e
variables (ISum e1 e2) = variables e1 ++ variables e2
variables (IProd e1 e2) = variables e1 ++ variables e2
variables (IDiv e1 e2) = variables e1 ++ variables e2
variables (IMod e1 e2) = variables e1 ++ variables e2
variables (IPow e1 e2) = variables e1 ++ variables e2

replace :: Object IEX -> String -> Object IEX -> Object IEX
replace (IConst c) _ _ = IConst c
replace (IVar n) name value
  | n == name = value
  | otherwise = IVar n
replace (INeg e) name value = INeg (replace e name value)
replace (ISum e1 e2) name value = ISum (replace e1 name value) (replace e2 name value)
replace (IProd e1 e2) name value = IProd (replace e1 name value) (replace e2 name value)
replace (IDiv e1 e2) name value = IDiv (replace e1 name value) (replace e2 name value)
replace (IMod e1 e2) name value = IMod (replace e1 name value) (replace e2 name value)
replace (IPow e1 e2) name value = IPow (replace e1 name value) (replace e2 name value)
