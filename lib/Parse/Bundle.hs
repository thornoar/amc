{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Parse.Bundle (ParseResult, parseResult) where

import Object.Bundle
import Data.Kind (Constraint)
import Result
import Description

import qualified Parse.Instances.ParseIEX as PIEX
import qualified Parse.Instances.ParseREX as PREX

type ParseResult :: ObjectTag -> Constraint
class ParseResult tg where
  parseResult :: String -> Result (Object tg)

instance {-# OVERLAPPABLE #-} Description tg => ParseResult tg where
  parseResult _ = let res = Error $ description (proxyOf2 res) ++ " cannot be parsed" in res

instance {-# OVERLAPPING #-} ParseResult IEX where
  parseResult = PIEX.parse

instance {-# OVERLAPPING #-} ParseResult REX where
  parseResult = PREX.parse

instance {-# OVERLAPPING #-} ParseResult STR where
  parseResult = Content . Raw
