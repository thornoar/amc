{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Object where

data ObjectTag = EXP

data Object a where
  Const :: String -> Object EXP
  Sum :: Object EXP -> Object EXP -> Object EXP
  Prod :: Object EXP -> Object EXP -> Object EXP
  Diff :: Object EXP -> Object EXP -> Object EXP
