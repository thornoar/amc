{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import System.Console.Haskeline

import Action.Bundle
import Object.Bundle

import Result
import Input
import Data.Char (toUpper)
import Data.List (intercalate, intersperse)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Text.Read (readMaybe)
import Data.List.Unique (uniq)
import Control.Monad (when)

handle :: IIO () -> IIO ()
handle act = let lp = handleInterrupt lp act in lp

loop :: Bool -> (ObjectTag, [ActionTag]) -> IIO ()
loop pm p@(ot, ats) = do
  -- minput <- getColoredInputLine $ getPrompt '#' (show ot) (show at)
  when pm $ outputStrLn $ "(" ++ color "34" "object" ++ ": " ++ color "33" (show ot) ++ "; " ++ color "34" "actions" ++ ": " ++ color "33" (concat (intersperse " | " $ map show ats)) ++ ")"
  minput <- getColoredInputLine $ "#####: "
  let process :: (a -> IIO ()) -> Result a -> IIO ()
      process = switch $ (>> loop True p) . printError
      helpAction = (>> loop True p) . outputStr . unlines $
        ("Available objects: " ++ intercalate ", " (map show allObjectTags)) :
        ("Available actions: " ++ intercalate ", " (map show allActionTags)) :
        []
  flip (switch (const $ return ())) minput $ \input -> case input of
    [] -> loop False p
    ('s':'e':'t':' ':rest) ->
      let mp' = case (splitPair (== '|') rest) of
            (otstr, "") -> (,ats) <$> guessElement otstr allObjectTags
            ("", atstr) -> (ot,) <$> parseActionChain [] atstr
            (otstr, atstr) -> (,) <$> guessElement otstr allObjectTags <*> parseActionChain [] atstr
       in process (\p' -> handle $ loop True p') mp'
    "help" -> helpAction
    "?" -> helpAction
    "exit" -> return ()
    _ -> let (objstr, pipes) = splitPair (== '|') input
          in parseByTag ot objstr $ process $
            \obj -> process
              (\ats' -> actionChain ats' (loop True p) obj)
              (fmap (uniq . intersperse' PRINT) $ parseActionChain ats pipes)

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
  runInputT settings $ withInterrupt $ handle $ loop True (ot, [at])
