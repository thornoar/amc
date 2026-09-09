{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Parse.Rnstances.ParseREX where
import Result
import Object.Bundle
import Object.RealNumber
import Data.List (elemIndex)
import Data.Ratio
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Text.Read (readMaybe)

type Output = Result (Object REX, String)

mkError :: String -> Result a
mkError msg = Error ("could not parse real expression: " ++ msg)

parse :: String -> Result (Object REX)
parse = todo

parseSum :: String -> Output
parseSum src = parseProdDiv src >>= uncurry go
  where
  go :: Object REX -> String -> Output
  go !obj ('+' : src) = parseProdDiv src >>= \ (obj', src) -> go (RSum obj obj') src
  go !obj ('-' : src) = parseProdDiv src >>= \ (obj', src) -> go (RDiff obj obj') src
  go !obj src = Content (obj, src)
  
parseProdDiv :: String -> Output
parseProdDiv src = parseExponent src >>= uncurry go
  where
  go :: Object REX -> String -> Output
  go !obj ('*' : src) = parseExponent src >>= \ (obj', src) -> go (RProd obj obj') src
  go !obj ('/' : src) = parseExponent src >>= \ (obj', src) -> go (RDiv obj obj') src
  go !obj src = Content (obj, src)

parseExponent :: String -> Output
parseExponent src = parseSimple src >>= \ (obj, src) ->
  case src of
    '^' : src -> parseExponent src >>= \ (obj', src) -> Content (RPow obj obj', src)
    _ -> Content (obj, src)

takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile _ [] = ([], [])
takeDropWhile cond lst@(a : rest)
  | cond a = let (taken, dropped) = takeDropWhile cond rest in (a : taken, dropped)
  | otherwise = ([], lst)

parseSimple :: String -> Output
parseSimple ('-' : src) = parseSimple src >>= \ (obj, src) -> Content (RNeg obj, src)
parseSimple ('(' : src) = parseSum src >>= \ (obj, src) -> case src of
  ')' : src -> Content (obj, src)
  _ -> mkError "unclosed parenthesis"
parseSimple (a : rest)
  | isAlpha a = let (rname, src) = takeDropWhile isAlphaNum rest in Content (RVar (a : rname), src)
  | isDigit a = let (rconst, src) = takeDropWhile isDigit rest in case readMaybe (a : rconst) of
      Just num -> Content (RConst num, src)
      Nothing -> mkError $ "could not read `" ++ (a : rconst) ++ "` as an integer constant"
  | otherwise = mkError $ "unexpected character: `" ++ show a ++ "`"
parseSimple [] = mkError "expected an expression"


parseRN :: String -> Result RealNumber
parseRN [] = mkError "expected a constant"
parseRN "e" = Content E
parseRN "pi" = Content PI
parseRN "gamma" = Content GAMMA
parseRN "ln(2)" = Content LN2
parseRN str = case elemIndex '/' str of
  Nothing -> Dbl <$> readResult str
  Just i -> let (s1,s2) = splitAt i str
             in fmap Rt $ (%) <$> readResult s1 <*> readResult (drop 1 s2)
