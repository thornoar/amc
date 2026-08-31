{-# OPTIONS_GHC -Wno-name-shadowing #-}
module ParseIEX (parse) where

import Object
import Result
import Data.Char (isSpace)

mkError :: String -> Result a
mkError msg = Error ("could not parse integer: " ++ msg)

type Output = Result (Object IEX, String)

parse :: String -> Result (Object IEX)
parse src = parseSum (filter (not . isSpace) src) >>= \ (obj, src) ->
  case src of
    [] -> Content obj
    _ -> mkError $ "unexpected input continuation: `" ++ src ++ "`"


parseSum :: String -> Output
parseSum src = parseProdDiv src >>= \ (obj1, src) -> case src of
  '+' : src -> parseSum src >>= \ (obj2, src) -> 
    -- rest src >>= \ (objs, src) -> Content (ISum (obj : objs), src)
  -- where
  -- -- mfirst = parseProdDiv src
  -- rest :: String -> Result ([Object IEX], String)
  -- rest ('+' : src) =
  --   parseProdDiv src >>= \ (obj, src) ->
  --     rest src >>= \ (objs, src) -> Content (obj : objs, src)
  -- rest [] = Content ([], [])
  -- rest src = mkError $ "unexpected input continuation: `" ++ src ++ "`"
  
parseProdDiv :: String -> Output
parseProdDiv = undefined

parseExponent :: String -> Output
parseExponent = undefined


