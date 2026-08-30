{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Print (PrintResult, printResult) where

import Object
import Data.Kind (Constraint)
-- import Description
import Result
import Description

type Print :: ObjectTag -> Constraint
class Print tg where
  print_ :: Object tg -> String
  -- default print :: Show (Object tg) => Object tg -> String
  -- print = show

instance {-# OVERLAPPING #-} PrintResult IEX
instance Print IEX where
  print_ = show

instance {-# OVERLAPPING #-} PrintResult REX
instance Print REX where
  print_ = show

instance {-# OVERLAPPING #-} PrintResult STR
instance Print STR where
  print_ (Raw str) = str

type PrintResult :: ObjectTag -> Constraint
class PrintResult tg where
  printResult :: Object tg -> Result (Object STR)
  default printResult :: Print tg => Object tg -> Result (Object STR)
  printResult = Content . Raw . print_
instance {-# OVERLAPPABLE #-} Description tg => PrintResult tg where
  printResult obj = Error $ (description (proxyOf obj)) ++ " cannot be printed"
