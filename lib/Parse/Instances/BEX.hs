{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
module Parse.Instances.BEX (parse) where

import Object.Bundle
import Result
import Data.Char (isAlpha, isAlphaNum)

type Output = Result (Object BEX, String)

mkError :: String -> Result a
mkError msg = Error ("could not parse boolean expression: " ++ msg)

parse :: String -> Result (Object BEX)
parse str = parseEquiv str >>= \ (obj, rest) -> case rest of
  [] -> Content obj
  _ -> mkError $ "unexpected continuation: `" ++ rest ++ "`"

parseEquiv :: String -> Output
parseEquiv src = parseImpl src >>= uncurry go
  where
  go :: Object BEX -> String -> Output
  go !obj (' ' : src) = go obj src
  go !obj ('<' : '=' : '>' : src) = parseImpl src >>= \ (obj', src) -> go (BEquiv obj obj') src
  go !obj src = Content (obj, src)

parseImpl :: String -> Output
parseImpl src = parseOr src >>= uncurry go
  where
  go :: Object BEX -> String -> Output
  go !obj (' ' : src) = go obj src
  go !obj ('=' : '>' : src) = parseOr src >>= \ (obj', src) -> go (BImpl obj obj') src
  go !obj ('<' : '=' : '>' : src) = parseOr src >>= \ (obj', src) -> go (BEquiv obj obj') src
  go !obj ('<' : '=' : src) = parseOr src >>= \ (obj', src) -> go (BImpl obj' obj) src
  go !obj src = Content (obj, src)

parseOr :: String -> Output
parseOr src = parseXor src >>= uncurry go
  where
  go :: Object BEX -> String -> Output
  go !obj (' ' : src) = go obj src
  go !obj ('|' : src) = parseXor src >>= \ (obj', src) -> go (BOr obj obj') src
  go !obj src = Content (obj, src)

parseXor :: String -> Output
parseXor src = parseAnd src >>= uncurry go
  where
  go :: Object BEX -> String -> Output
  go !obj (' ' : src) = go obj src
  go !obj ('^' : src) = parseAnd src >>= \ (obj', src) -> go (BXor obj obj') src
  go !obj src = Content (obj, src)

parseAnd :: String -> Output
parseAnd src = parseNot src >>= uncurry go
  where
  go :: Object BEX -> String -> Output
  go !obj (' ' : src) = go obj src
  go !obj ('&' : src) = parseNot src >>= \ (obj', src) -> go (BAnd obj obj') src
  go !obj src = Content (obj, src)

parseNot :: String -> Output
parseNot (' ' : src) = parseNot src
parseNot ('!' : src) = parseNot src >>= \ (obj, src) -> Content (BNot obj, src)
parseNot ('¬' : src) = parseNot src >>= \ (obj, src) -> Content (BNot obj, src)
parseNot src = parseSimple src

takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile _ [] = ([], [])
takeDropWhile cond lst@(a : rest)
  | cond a = let (taken, dropped) = takeDropWhile cond rest in (a : taken, dropped)
  | otherwise = ([], lst)

parseSimple :: String -> Output
parseSimple (' ' : src) = parseSimple src
parseSimple ('(' : src) = parseEquiv src >>= \ (obj, src) -> case src of
  ')' : src -> Content (obj, src)
  _ -> mkError "unclosed parenthesis"
parseSimple ('1' : rest) = Content (BConst True, rest)
parseSimple ('0' : rest) = Content (BConst False, rest)
parseSimple ('t' : 'r' : 'u' : 'e' : rest) = Content (BConst True, rest)
parseSimple ('T' : 'R' : 'U' : 'E' : rest) = Content (BConst True, rest)
parseSimple ('f' : 'a' : 'l' : 's' : 'e' : rest) = Content (BConst False, rest)
parseSimple ('F' : 'A' : 'L' : 'S' : 'E' : rest) = Content (BConst False, rest)
parseSimple (a : rest)
  | isAlpha a = let (rname, src) = takeDropWhile isAlphaNum rest in Content (BVar (a : rname), src)
  | otherwise = mkError $ "unexpected character: " ++ show a
parseSimple [] = mkError "expected an expression"
