{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import System.Console.Haskeline

import Action.Bundle
import Object.Bundle

import Result
import Input
import Data.Char (isSpace, toUpper)
import Data.List (intercalate)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Text.Read (readMaybe)

handle :: IIO () -> IIO ()
handle act = let lp = handleInterrupt lp act in lp

loop :: (ObjectTag, ActionTag) -> IIO ()
loop p@(ot, at) = do
  minput <- getColoredInputLine $ getPrompt '#' (show ot) (show at)
  let process :: (a -> IIO ()) -> Result a -> IIO ()
      process = switch $ (>> loop p) . printError
      helpAction = (>> loop p) . outputStr . unlines $
        ("Available objects: " ++ intercalate ", " (map show allObjectTags)) :
        ("Available actions: " ++ intercalate ", " (map show allActionTags)) :
        []
  flip (switch (const $ return ())) minput $ \input -> case input of
    [] -> loop p
    ('s':'e':'t':' ':rest) ->
      let mp' = case (split (filter (not . isSpace) rest) '>') of
            [otstr] -> (,at) <$> guessElement otstr allObjectTags
            ["",atstr] -> (ot,) <$> guessElement atstr allActionTags
            [otstr, atstr] -> (,) <$> guessElement otstr allObjectTags <*> guessElement atstr allActionTags
            _ -> Error "invalid syntax for setting object/action modes"
       in process (\p' -> handle $ loop p') mp'
    "help" -> helpAction
    "?" -> helpAction
    "exit" -> return ()
    _ -> parseByTag ot input $ process $
      \ (obj, rest) -> flip process (parseActionChain at rest) $
        \ats -> actionChain ats (loop p) obj

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just ".amc-history",
  autoAddHistory = True
}

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  let ot = fromMaybe REX $ M.lookup ObjectOpt args >>= (readMaybe . map toUpper)
      at = fromMaybe SIMPL $ M.lookup ActionOpt args >>= (readMaybe . map toUpper)
  runInputT settings $ withInterrupt $ handle $ loop (ot, at)
