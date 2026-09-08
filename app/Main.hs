{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import System.Console.Haskeline

import Action.Bundle
import Object.Bundle
import Parse.Bundle
import Display.Bundle

import Result
import Input
import Data.Char (isSpace, toUpper)

loop :: (ObjectTag, ActionTag) -> [String] -> InputT IO ()
loop p@(ot, at) history = do
  minput <- getColoredInputLine $ getPrompt '#' (show ot) (show at)
  let process :: (a -> InputT IO ()) -> Result a -> InputT IO ()
      process = switch ((>> loop p history) . printError)
  flip (switch (const $ return ())) minput $ \input -> case input of
    [] -> loop p history
    ('s':'e':'t':' ':rest) ->
      let mp' = case (split (map toUpper . filter (not . isSpace) $ rest) '>') of
            [otstr] -> (,at) <$> readResult otstr
            ["",atstr] -> (ot,) <$> readResult atstr
            [otstr, atstr] -> (,) <$> readResult otstr <*> readResult atstr
            _ -> Error "invalid syntax for setting object/action modes"
       in process (\p' -> loop p' history) mp'
    "help" -> todo
    "exit" -> return ()
    _ -> byTag ot parseResult input $ process $
         action at $ process $
         process ((>> loop p history) . outputStrLn) . displayResult

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just ".amc-history",
  autoAddHistory = True
}

main :: IO ()
main = runInputT settings (loop (IEX, SHOW) [])
