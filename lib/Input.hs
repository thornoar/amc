{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Input where

import System.Console.Haskeline
import Result
import Data.Map (Map, empty, insertWith)
import Data.Char (toUpper)
import Data.List (isPrefixOf)

data OptionName = ObjectOpt | ActionOpt
  deriving (Read, Show, Eq, Ord)

type Arguments = Map OptionName String

type Process a = IIO (Result a)

type IIO = InputT IO

removePrefix :: (a -> Bool) -> [a] -> [a]
removePrefix test (c:rest)
  | test c = removePrefix test rest
  | otherwise = c:rest
removePrefix _ [] = []

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

splitPair :: Eq a => (a -> Bool) -> [a] -> ([a],[a])
splitPair _ [] = ([],[])
splitPair test (a:rest)
  | test a = ([], rest)
  | otherwise = let (lft, rgt) = splitPair test rest in (a : lft, rgt)

printError :: String -> IIO ()
printError msg = outputStrLn (color "31" "Error:" ++ " " ++ msg)

insert' :: Ord k => k -> a -> Map k a -> Map k a
insert' = insertWith (\_ b -> b)

parseArgs :: [String] -> Arguments
parseArgs [] = empty
parseArgs ("-a":str:rest) = insert' ActionOpt str $ parseArgs rest
parseArgs ("-o":str:rest) = insert' ObjectOpt str $ parseArgs rest
parseArgs (_:rest) = parseArgs rest

guessElement :: Show a => String -> [a] -> Result a
guessElement str lst = foldl go (Error $ "could not match prefix: `" ++ capStr ++ "`") lst
  where
    capStr = map toUpper str
    go :: Show a => Result a -> a -> Result a
    go prev@(Content _) _ = prev
    go prev a
      | capStr `isPrefixOf` (map toUpper $ show a) = Content a
      | otherwise = prev

intersperse' :: Eq a => a -> [a] -> [a]
intersperse' _ [] = []
intersperse' a (a':rest) = a' : a : intersperse' a rest
