module Input where

import System.Console.Haskeline
import Result

type Process a = InputT IO (Result a)

color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m" -- ]]

promptLength :: Int
promptLength = 18

getPrompt :: Char -> String -> String
getPrompt rep body = "\ESC[0m(" ++ color "33" body ++ ") " ++ replicate n rep ++ ": \ESC[34m" -- ]]
  where n = promptLength - length body - 5

getColoredInputLine :: String -> Process String
getColoredInputLine pref = do
  res <- getInputLine pref
  outputStr "\ESC[0m" -- ]
  case res of
    Nothing -> return (Error "Input error")
    Just str -> return (Content str)

printError :: String -> InputT IO ()
printError msg = outputStrLn (color "31" "Error:" ++ " " ++ msg)
