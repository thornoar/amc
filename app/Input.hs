module Input where

import System.Console.Haskeline
import Result

type Process a = InputT IO (Result a)

color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m" -- ]]

promptLength :: Int
promptLength = 20

getPrompt :: Char -> String -> String -> String
getPrompt rep body1 body2 = "\ESC[0m(" ++ color "33" body1 ++ " -> " ++ color "33" body2 ++ ") " ++ replicate n rep ++ ": \ESC[34m" -- ]]
  where n = promptLength - length body1 - length body2 - 9

getColoredInputLine :: String -> Process String
getColoredInputLine pref = do
  res <- getInputLine pref
  outputStr "\ESC[0m" -- ]
  case res of
    Nothing -> return (Error "no input")
    Just str -> return (Content str)

split :: Eq a => [a] -> a -> [[a]]
split [] _ = [[]]
split (a:as) delim
  | a == delim = [] : split as delim
  | otherwise = case split as delim of
      (bs:bss) -> (a:bs) : bss
      [] -> [[a]]

printError :: String -> InputT IO ()
printError msg = outputStrLn (color "31" "Error:" ++ " " ++ msg)
