{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import System.Console.Haskeline
import Action
import Object
import Result
import Input
import Parse

parseByTag
  :: ObjectTag
  -> Object STR
  -> (forall tg. ParseResult tg => Result (Object tg) -> InputT IO ())
  -> InputT IO ()
parseByTag IEX raw cont = cont (parseResult raw :: Result (Object IEX))
parseByTag REX raw cont = cont (parseResult raw :: Result (Object REX))
parseByTag STR raw cont = cont (parseResult raw :: Result (Object STR))

loop :: (ObjectTag, ActionTag) -> [String] -> InputT IO ()
loop p@(ot, at) history = do
  minput <- getColoredInputLine $ getPrompt '#' ">"
  case minput of
    Error msg -> printError msg >> loop p history
    Content input -> parseByTag ot (Raw input) $ \parsed -> case parsed of
      Error msg -> printError msg >> loop p history
      Content obj -> case at of
        SIMPL -> undefined obj

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just ".lambda-interpreter-history",
  autoAddHistory = True
}

main :: IO ()
main = runInputT settings (loop (IEX, SIMPL) [])
