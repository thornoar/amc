{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Action.Bundle where

-- import Simplify
-- import Parse
-- import Data.Kind (Constraint)
import Object.Bundle
import Result
import Data.Kind (Constraint)
import Action.Simplify
import Display.Bundle
import Parse.Bundle
import Description

data ActionTag = SIMPL | RETURN | SHOW deriving (Show, Read)
instance Description ActionTag where description _ = "an action tag"

type AllActions :: ObjectTag -> Constraint
type family AllActions tg where
  AllActions tg = (
      Description tg,
      DisplayResult tg,
      ParseResult tg,
      SimplifyResult tg
    )

byTag ::
  ObjectTag ->
  (forall (tg :: ObjectTag). AllActions tg => a -> Result (Object tg)) ->
  a ->
  (forall tg. AllActions tg => Result (Object tg) -> b) ->
  b
byTag IEX f a cont = cont (f a :: Result (Object IEX))
byTag REX f a cont = cont (f a :: Result (Object REX))
byTag STR f a cont = cont (f a :: Result (Object STR))

action :: AllActions tg =>
  ActionTag ->
  (forall tg'. AllActions tg' => Result (Object tg') -> a) ->
  Object tg ->
  a
action SIMPL cont obj = cont (simplifyResult obj)
action RETURN cont obj = cont (Content obj)
action SHOW cont obj = cont (Content $ Raw (show obj))
