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
import Input (IIO, printError, removePrefix, splitPair, guessElement)
import Data.Char (isSpace)

data ActionTag = SIMPL | RETURN | SHOW | PRINT
  deriving (Show, Read, Enum, Bounded)

allActionTags :: [ActionTag]
allActionTags = enumFrom minBound

parseActionChain :: ActionTag -> String -> Result [ActionTag]
parseActionChain ini str = case removePrefix isSpace str of
  [] -> Content [ini, PRINT]
  c:rest -> let (atstr, other) = splitPair isSpace rest in case guessElement atstr allActionTags of
    Content at -> case c of
      '@' -> fmap (at:) $ parseActionChain ini other
      '>' -> fmap (ini:) $ parseActionChain at other
      _ -> Error $ "unrecognized pipe symbol: " ++ show c
    Error msg -> Error ("processing characters left after parsing: " ++ msg)

instance Description ActionTag where description _ = "an action tag"

type AllActions :: ObjectTag -> Constraint
type family AllActions tg where
  AllActions tg = (
      Description tg,
      DisplayResult tg,
      ParseResult tg,
      SimplifyResult tg
    )

parseByTag ::
  ObjectTag ->
  -- (forall (tg :: ObjectTag). ParseResult tg => String -> Result (Object tg, String)) ->
  String ->
  (forall tg. AllActions tg => Result (Object tg, String) -> b) ->
  b
parseByTag IEX a cont = cont (parseResult a :: Result (Object IEX, String))
parseByTag REX a cont = cont (parseResult a :: Result (Object REX, String))
parseByTag STR a cont = cont (parseResult a :: Result (Object STR, String))

action :: AllActions tg =>
  ActionTag ->
  (forall tg'. AllActions tg' => Result (Object tg') -> IIO a) ->
  Object tg ->
  IIO a
action SIMPL cont obj = simplifyResult obj >>= cont
action RETURN cont obj = cont (Content obj)
action SHOW cont obj = cont (Content $ Raw (show obj))
action PRINT cont obj = displayResult obj >>= cont

actionChain :: AllActions tg =>
  [ActionTag] ->
  -- (forall tg'. AllActions tg' => Result (Object tg') -> IIO ()) ->
  IIO () ->
  Object tg ->
  IIO ()
actionChain [] cont _ = cont
actionChain (at:rest) cont obj = action at f obj
  where
    f :: forall tg'. AllActions tg' => Result (Object tg') -> IIO ()
    f (Error msg) = printError msg >> cont
    f (Content newobj) = actionChain rest cont newobj
