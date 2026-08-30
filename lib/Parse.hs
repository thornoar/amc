{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Parse (ParseResult, parseResult) where

import Object
import Data.Kind (Constraint)
import Result
import Description

import qualified ParseIEX

type ParseResult :: ObjectTag -> Constraint
class ParseResult tg where
  parseResult :: String -> Result (Object tg)
  default parseResult :: Description tg => String -> Result (Object tg)
  parseResult _ = let res = Error $ description (proxyOf2 res) ++ " cannot be parsed" in res
instance {-# OVERLAPPABLE #-} Description tg => ParseResult tg

instance {-# OVERLAPPING #-} ParseResult IEX where
  parseResult = ParseIEX.parse
