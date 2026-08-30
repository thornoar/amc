{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Action where

-- import Simplify
-- import Parse
-- import Data.Kind (Constraint)
import Object
import Result
import Data.Kind (Constraint)
import Simplify
import Print
import Parse
import Description

data ActionTag = SIMPL | PRINT

type AllActions :: ObjectTag -> Constraint
type family AllActions tg where
  AllActions tg = (
      Description tg,
      PrintResult tg,
      ParseResult tg,
      SimplifyResult tg
    )

byTag ::
  ObjectTag ->
  (forall (tg :: ObjectTag). AllActions tg => a -> Result (Object tg)) ->
  a ->
  (forall tg. AllActions tg => Result (Object tg) -> b) -> b
byTag IEX f a cont = cont (f a :: Result (Object IEX))
byTag REX f a cont = cont (f a :: Result (Object REX))
byTag STR f a cont = cont (f a :: Result (Object STR))

action :: AllActions tg =>
  ActionTag ->
  Object tg ->
  (forall tg'. AllActions tg' => Result (Object tg') -> a) ->
  a
action SIMPL obj cont = cont (simplifyResult obj)
action PRINT obj cont = cont (printResult obj)
