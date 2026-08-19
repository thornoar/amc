{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Object where

data ObjectTag = REX | IEX

data Object a where
  IConst :: Integer -> Object IEX
