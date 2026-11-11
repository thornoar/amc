module Action.Substitute.Instances.BEX (substituteResult) where

import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.List (nub)

import Input (IIO, getColoredInputLine, promptLength)
import Object.Bundle
import Parse.Instances.BEX (parse)
import Result

substituteResult :: Object BEX -> IIO (Result (Object BEX))
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
  foldM step (Content obj) vars

variables :: Object BEX -> [String]
variables (BConst _) = []
variables (BVar name) = [name]
variables (BNot e) = variables e
variables (BAnd e1 e2) = variables e1 ++ variables e2
variables (BOr e1 e2) = variables e1 ++ variables e2
variables (BXor e1 e2) = variables e1 ++ variables e2
variables (BImpl e1 e2) = variables e1 ++ variables e2
variables (BEquiv e1 e2) = variables e1 ++ variables e2

replace :: Object BEX -> String -> Object BEX -> Object BEX
replace (BConst c) _ _ = BConst c
replace (BVar n) name value
  | n == name = value
  | otherwise = BVar n
replace (BNot e) name value = BNot (replace e name value)
replace (BAnd e1 e2) name value = BAnd (replace e1 name value) (replace e2 name value)
replace (BOr e1 e2) name value = BOr (replace e1 name value) (replace e2 name value)
replace (BXor e1 e2) name value = BXor (replace e1 name value) (replace e2 name value)
replace (BImpl e1 e2) name value = BImpl (replace e1 name value) (replace e2 name value)
replace (BEquiv e1 e2) name value = BEquiv (replace e1 name value) (replace e2 name value)
