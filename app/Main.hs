module Main (main) where

import System.Console.Haskeline
import Actions
import Object
import Result
import Action

loop :: (ObjectTag, ActionTag) -> [String] -> InputT IO ()
loop p@(ot, at) history = do
  minput <- getColoredInputLine $ getPrompt '#' ">"
  case minput of
    Error str -> printError str >> loop p history
    Content input -> outputStrLn input >> loop p history

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just ".lambda-interpreter-history",
  autoAddHistory = True
}

main :: IO ()
main = runInputT settings (loop (IEX, SIMPL) [])
