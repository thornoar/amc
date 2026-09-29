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
import Data.List (intercalate)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Text.Read (readMaybe)

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
    "help" -> (>> loop p history) . outputStr . unlines $
      ("Available objects: " ++ intercalate ", " (map show allObjectTags)) :
      ("Available actions: " ++ intercalate ", " (map show allActionTags)) :
      []
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
main = do
  args <- parseArgs <$> getArgs
  let ot = fromMaybe IEX $ M.lookup ObjectOpt args >>= (readMaybe . map toUpper)
      at = fromMaybe SIMPL $ M.lookup ActionOpt args >>= (readMaybe . map toUpper)
  runInputT settings (loop (ot, at) [])
