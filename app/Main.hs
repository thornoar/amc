{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import System.Console.Haskeline

import Action.Bundle
import Object.Bundle

import Result
import Input
import Data.Char (isSpace, isNumber)
import Data.List (intercalate, intersperse)
import System.Environment (getArgs)
import qualified Data.Map as M
import Control.Monad (when)

handle :: IIO () -> IIO ()
handle act = let lp = handleInterrupt lp act in lp

loop :: Bool -> (ObjectTag, [ActionTag]) -> [String] -> IIO ()
loop pm p@(ot, ats) hist = do
  let actstr = if null ats then color "33" "none" else (concat (intersperse " > " $ map (color "33" . showActionTag) ats))
  when pm $ outputStrLn $ "\n(" ++ color "34" "object" ++ ": " ++ color "33" (showObjectTag ot) ++ "; " ++ color "34" "actions" ++ ": " ++ actstr ++ ")"
  minput <- getColoredInputLine $ replicate (promptLength - 2) '#' ++ ": "
  let process :: (a -> IIO ()) -> Result a -> IIO ()
      process = switch $ (>> loop True p hist) . printError
      helpAction = (>> loop True p hist) . outputStr . unlines $
        ("\n Available objects:\n" ++ concat (map ((++ "\n") . ("   " ++) . showObjectTag) allObjectTags)) :
        (" Supported actions:\n" ++ concat (map ((++ "\n") . ("   " ++) . showActionTag) allActionTags)) :
        (" Pipe symbols: " ++ intercalate ", " (map show pipeSymbols) ++ "\n") :
        (" Syntax for previous answers: ~[num]") :
        []
      replaceAns :: String -> Result String
      replaceAns ('~':rest) =
        let (mind, len) = case takeWhile isNumber rest of
              [] -> (Content 1, 0)
              digits -> (readResult digits, length digits)
            getAns :: Int -> Result String
            getAns ind
              | ind > length hist = Error "history index out of bounds"
              | ind < 1 = Error "index must be non-negative"
              | otherwise = Content (hist !! (ind - 1))
         in (++) <$> (mind >>= getAns) <*> (replaceAns (drop len rest))
      replaceAns (c:rest) = (c:) <$> replaceAns rest
      replaceAns "" = Content ""
  flip (switch (const $ return ())) minput $ \input -> case input of
    [] -> loop False p hist
    ('s':'e':'t':rest) -> case filter (not . isSpace) rest of
      "" -> loop True p hist
      "none" -> loop True (ot, []) hist
      str -> let mp' = case (splitPair True (`elem` pipeSymbols) str) of
                   (otstr, "") -> (,ats) <$> guessElement showObjectTag otstr allObjectTags
                   ("", atstr) -> (ot,) <$> parseActionChain ats atstr
                   (otstr, atstr) -> (,) <$> guessElement showObjectTag otstr allObjectTags <*> parseActionChain ats atstr
              in process (\p' -> handle $ loop True p' hist) mp'
    "help" -> helpAction
    "?" -> helpAction
    "exit" -> return ()
    _ -> let (objstr, pipes) = splitPair True (`elem` pipeSymbols) input
          in flip process (replaceAns objstr) $ \str -> (parseByTag ot str) $ process $
            \obj -> flip process (parseActionChain ats pipes) $
              \ats' -> actionChain ats' (loop True p) obj hist
              

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
    Content (ot,ats) -> runInputT settings $ withInterrupt $ handle $ loop True (ot, ats) []
