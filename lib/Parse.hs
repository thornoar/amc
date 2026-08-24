{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Parse where

import Object
import Data.Kind (Constraint)
import Result
import Description

type ParseResult :: ObjectTag -> Constraint
class ParseResult tg where
  parseResult :: Object STR -> Result (Object tg)
  default parseResult :: Description tg => Object STR -> Result (Object tg)
  parseResult _ = let res = Error $ description (proxyOf2 res) ++ " cannot be parsed" in res

instance ParseResult IEX where
  parseResult _ = Content (IConst 0)

instance {-# OVERLAPPABLE #-} Description tg => ParseResult tg
