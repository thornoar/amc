{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
module ParseIEX where

import Object
import Result
import Data.Char (isSpace, isAlpha, isAlphaNum, isDigit)
import Text.Read (readMaybe)

mkError :: String -> Result a
mkError msg = Error ("could not parse integer: " ++ msg)

type Output = Result (Object IEX, String)

parse :: String -> Result (Object IEX)
parse src = parseSum (filter (not . isSpace) src) >>= \ (obj, src) ->
  case src of
    [] -> Content obj
    _ -> mkError $ "unexpected input continuation: `" ++ src ++ "`"


parseSum :: String -> Output
parseSum src = parseProdDivMod src >>= uncurry go
  where
  go :: Object IEX -> String -> Output
  go !obj ('+' : src) = parseProdDivMod src >>= \ (obj', src) -> go (ISum obj obj') src
  go !obj ('-' : src) = parseProdDivMod src >>= \ (obj', src) -> go (ISum obj (INeg obj')) src
  go !obj src = Content (obj, src)
  
parseProdDivMod :: String -> Output
parseProdDivMod src = parseExponent src >>= uncurry go
  where
  go :: Object IEX -> String -> Output
  go !obj ('*' : src) = parseExponent src >>= \ (obj', src) -> go (IProd obj obj') src
  go !obj ('/' : src) = parseExponent src >>= \ (obj', src) -> go (IDiv obj obj') src
  go !obj ('%' : src) = parseExponent src >>= \ (obj', src) -> go (IMod obj obj') src
  go !obj src = Content (obj, src)

parseExponent :: String -> Output
parseExponent src = parseSimple src >>= \ (obj, src) ->
  case src of
    '^' : src -> parseExponent src >>= \ (obj', src) -> Content (IPow obj obj', src)
    _ -> Content (obj, src)

takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile _ [] = ([], [])
takeDropWhile cond lst@(a : rest)
  | cond a = let (taken, dropped) = takeDropWhile cond rest in (a : taken, dropped)
  | otherwise = ([], lst)

parseSimple :: String -> Output
parseSimple ('-' : src) = parseSimple src >>= \ (obj, src) -> Content (INeg obj, src)
parseSimple ('(' : src) = parseSum src >>= \ (obj, src) -> case src of
  ')' : src -> Content (obj, src)
  _ -> mkError "unclosed parenthesis"
parseSimple (a : rest)
  | isAlpha a = let (rname, src) = takeDropWhile isAlphaNum rest in Content (IVar (a : rname), src)
  | isDigit a = let (rconst, src) = takeDropWhile isDigit rest in case readMaybe (a : rconst) of
      Just num -> Content (IConst num, src)
      Nothing -> mkError $ "could not read `" ++ (a : rconst) ++ "` as an integer constant"
  | otherwise = mkError $ "unexpected character: `" ++ show a ++ "`"
parseSimple [] = mkError "expected an expression"
