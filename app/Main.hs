{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import System.Console.Haskeline

import Action.Bundle
import Object.Bundle

import Result
import Input
import Data.Char (isSpace)
import Data.List (intercalate, intersperse)
import System.Environment (getArgs)
import qualified Data.Map as M
import Data.List.Unique (uniq)
import Control.Monad (when)

handle :: IIO () -> IIO ()
handle act = let lp = handleInterrupt lp act in lp

loop :: Bool -> (ObjectTag, [ActionTag]) -> IIO ()
loop pm p@(ot, ats) = do
  let actstr = if null ats then color "33" "none" else (concat (intersperse " > " $ map (color "33" . showActionTag) ats))
  when pm $ outputStrLn $ "\n(" ++ color "34" "object" ++ ": " ++ color "33" (showObjectTag ot) ++ "; " ++ color "34" "actions" ++ ": " ++ actstr ++ ")"
  minput <- getColoredInputLine $ replicate (promptLength - 2) '#' ++ ": "
  let process :: (a -> IIO ()) -> Result a -> IIO ()
      process = switch $ (>> loop True p) . printError
      helpAction = (>> loop True p) . outputStr . unlines $
        ("\n Available objects: " ++ intercalate ", " (map showObjectTag allObjectTags)) :
        (" Supported actions: " ++ intercalate ", " (map showActionTag allActionTags)) :
        (" Pipe symbols: " ++ intercalate ", " (map show pipeSymbols)) :
        []
  flip (switch (const $ return ())) minput $ \input -> case input of
    [] -> loop False p
    ('s':'e':'t':rest) -> case filter (not . isSpace) rest of
      "" -> loop True p
      "none" -> loop True (ot, [])
      str -> let mp' = case (splitPair True (`elem` pipeSymbols) str) of
                   (otstr, "") -> (,ats) <$> guessElement showObjectTag otstr allObjectTags
                   ("", atstr) -> (ot,) <$> parseActionChain ats atstr
                   (otstr, atstr) -> (,) <$> guessElement showObjectTag otstr allObjectTags <*> parseActionChain ats atstr
              in process (\p' -> handle $ loop True p') mp'
    "help" -> helpAction
    "?" -> helpAction
    "exit" -> return ()
    _ -> let (objstr, pipes) = splitPair True (`elem` pipeSymbols) input
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
  let mot = case M.lookup ObjectOpt args of
        Nothing -> Content REX
        Just str -> guessElement showObjectTag str allObjectTags
      mats = case M.lookup ActionOpt args of
        Nothing -> Content [SIMPL]
        Just str -> parseActionChain [] str
  case (,) <$> mot <*> mats of
    Error msg -> printError' msg >> putStrLn ""
    Content (ot,ats) -> runInputT settings $ withInterrupt $ handle $ loop True (ot, ats)
