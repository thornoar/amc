{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Parse.Instances.ParseREX (parse) where
import Result
import Object.Bundle
import Object.RealNumber
import Data.Char (isAlpha, isAlphaNum, isDigit, toUpper)
import Text.Read (readMaybe)

type Output = Result (Object REX, String)

mkError :: String -> Result a
mkError msg = Error ("could not parse real expression: " ++ msg)

parse :: String -> Result (Object REX)
parse src = parseSum src >>= \ (obj, src) ->
  case src of
    [] -> Content obj
    _ -> mkError $ "unexpected input continuation: `" ++ src ++ "`"

parseSum :: String -> Output
parseSum src = parseProdDiv src >>= uncurry go
  where
  go :: Object REX -> String -> Output
  go !obj (' ' : src) = go obj src
  go !obj ('+' : src) = parseProdDiv src >>= \ (obj', src) -> go (RSum obj obj') src
  go !obj ('-' : src) = parseProdDiv src >>= \ (obj', src) -> go (RDiff obj obj') src
  go !obj src = Content (obj, src)
  
parseProdDiv :: String -> Output
parseProdDiv src = parseLog src >>= uncurry go
  where
  go :: Object REX -> String -> Output
  go !obj (' ' : src) = go obj src
  go !obj ('*' : src) = parseExponent src >>= \ (obj', src) -> go (RProd obj obj') src
  go !obj ('/' : src) = parseExponent src >>= \ (obj', src) -> go (RDiv obj obj') src
  go !obj src = Content (obj, src)

parseLog :: String -> Output
parseLog (' ':src) = parseLog src
parseLog ('l':'o':'g':'_':src) =
  parseSimple src >>= \ (base, src) ->
  parseExponent src >>= \ (ex, src) ->
  Content (RLog base ex, src)
parseLog src = parseExponent src

parseExponent :: String -> Output
parseExponent src = parseSimple src >>= \ (obj, src) ->
  let go :: String -> Output
      go (' ':src) = go src
      go ('^':src) = parseExponent src >>= \ (obj', src) -> Content (RPow obj obj', src)
      go src = Content (obj, src)
   in go src

takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile _ [] = ([], [])
takeDropWhile cond lst@(a : rest)
  | cond a = let (taken, dropped) = takeDropWhile cond rest in (a : taken, dropped)
  | otherwise = ([], lst)

parseSimple :: String -> Output
parseSimple (' ' : src) = parseSimple src
parseSimple ('-' : src) = parseSimple src >>= \ (obj, src) -> Content (RNeg obj, src)
parseSimple ('(' : src) = parseSum src >>= \ (obj, src) -> case src of
  ')' : src -> Content (obj, src)
  _ -> mkError "unclosed parenthesis"
parseSimple ('e':rest) = Content (RConst E, rest)
parseSimple ('p':'i':rest) = Content (RConst PI, rest)
parseSimple ('g':'a':'m':'m':'a':rest) = Content (RConst GAMMA, rest)
parseSimple (a : rest)
  | isAlpha a =
    let (rname, src) = takeDropWhile isAlphaNum rest
     in case readMaybe (toUpper a : rname) :: Maybe BuiltinFunction of 
          Just fun -> parseSimple src >>= \ (obj, src) -> Content (RApp fun obj, src)
          Nothing -> Content (RVar (a : rname), src)
  | isDigit a = let (rconst, src) = takeDropWhile isDigit rest in case src of
      '.':src -> let (rname', src') = takeDropWhile isDigit src in
        (\x -> (RConst (Dbl x), src')) <$> readResult (a : rconst ++ "." ++ rname')
      _ -> (\x -> (RConst (In x), src)) <$> readResult (a : rconst)
  | otherwise = mkError $ "unexpected character: `" ++ show a ++ "`"
parseSimple [] = mkError "expected an expression"

-- parseRN :: String -> Result RealNumber
-- parseRN [] = mkError "expected a constant"
-- parseRN "e" = Content E
-- parseRN "pi" = Content PI
-- parseRN "gamma" = Content GAMMA
-- parseRN str = if elem '.' str then Dbl <$> readResult str else In <$> readResult str
