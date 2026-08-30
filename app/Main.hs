{-# LANGUAGE RankNTypes #-}

module Main (main) where

import System.Console.Haskeline
import Action
import Object
import Result
import Input
import Parse
import Print

loop :: (ObjectTag, ActionTag) -> [String] -> InputT IO ()
loop p@(ot, at) history = do
  minput <- getColoredInputLine $ getPrompt '#' ">"
  case minput of
    Error msg -> printError msg >> loop p history
    Content input -> byTag ot parseResult input $ \parsed -> case parsed of
      Error msg -> printError msg >> loop p history
      Content obj -> action at obj $ \res -> case res of
        Error msg -> printError msg >> loop p history
        Content obj' -> case printResult obj' of
          Error msg -> printError msg >> loop p history
          Content (Raw str) -> outputStrLn str >> loop p history

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just ".lambda-interpreter-history",
  autoAddHistory = True
}

main :: IO ()
main = runInputT settings (loop (IEX, SIMPL) [])
