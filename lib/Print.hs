{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module Print where

import Object
import Data.Kind (Constraint)

type Print :: ObjectTag -> Constraint
class Print tg where
  print :: Object tg -> String
  default print :: Show (Object tg) => Object tg -> String
  print = show

instance Print IEX where
  print (IConst v) = show v
  print _ = "not implemented"

instance Show (Object tg) => Print tg
