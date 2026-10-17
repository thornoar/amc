{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Action.Bundle where

-- import Simplify
-- import Parse
-- import Data.Kind (Constraint)
import Object.Bundle
import Result
import Data.Kind (Constraint)
import Action.Simplify.Bundle
import Action.Display.Bundle
import Parse.Bundle
import Description
import Input (IIO, printError, splitPair, guessElement)
import Data.Char (isSpace)
import Action.Substitute.Bundle
import Action.Convert.Bundle
-- import Data.Char (isSpace)

data ActionTag = SIMPL | SUBS | SHOW | PRINT | CONV
  deriving (Show, Read, Eq, Enum, Bounded)

showActionTag :: ActionTag -> String
showActionTag SIMPL = "simplify"
showActionTag SUBS = "substitute"
showActionTag SHOW = "show"
showActionTag PRINT = "print"
showActionTag CONV = "convert"

allActionTags :: [ActionTag]
allActionTags = enumFrom minBound

vacuum :: String -> String
vacuum = filter (not . isSpace)

parseActionChain :: [ActionTag] -> String -> Result [ActionTag]
parseActionChain ats = fmap (uncurry (++)) . go ats . vacuum
  where
    go :: [ActionTag] -> String -> Result ([ActionTag], [ActionTag])
    go acc "" = Content (acc, [])
    go acc str = case splitPair (== '|') str of
      ('<':cur, rest) -> guessElement showActionTag cur allActionTags >>= \at -> go (at : acc) rest
      (cur, rest) -> go acc rest >>= \ (l,r) -> (\x -> (l, x:r)) <$> guessElement showActionTag cur allActionTags

-- parseActionChain str = case removePrefix isSpace str of
--   [] -> Content []
--   c:rest -> let (atstr, other) = splitPair isSpace (removePrefix isSpace rest) in case guessElement atstr allActionTags of
--     Content at -> case c of
--       '|' -> fmap (at:) $ parseActionChain other
--       _ -> Error $ "unrecognized pipe symbol: " ++ show c
--     Error msg -> Error ("processing characters left after parsing: " ++ msg)

instance Description ActionTag where description _ = "an action tag"

type AllActions :: ObjectTag -> Constraint
type family AllActions tg where
  AllActions tg = (
      Description tg,
      DisplayResult tg,
      ParseResult tg,
      SimplifyResult tg,
      SubstituteResult tg,
      ConvertResult tg
    )

parseByTag ::
  ObjectTag ->
  -- (forall (tg :: ObjectTag). ParseResult tg => String -> Result (Object tg, String)) ->
  String ->
  (forall tg. AllActions tg => Result (Object tg) -> b) ->
  b
parseByTag IEX a cont = cont (parseResult a :: Result (Object IEX))
parseByTag REX a cont = cont (parseResult a :: Result (Object REX))
parseByTag STR a cont = cont (parseResult a :: Result (Object STR))

action :: AllActions tg =>
  ActionTag ->
  (forall tg'. AllActions tg' => Result (Object tg') -> IIO a) ->
  Object tg ->
  IIO a
action SIMPL cont obj = simplifyResult obj >>= cont
action SUBS cont obj = substituteResult obj >>= cont
action SHOW cont obj = cont (Content $ Raw (show obj))
action PRINT cont obj = displayResult obj >>= cont
action CONV cont obj = convertResult obj >>= cont

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
