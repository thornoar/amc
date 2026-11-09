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
import Input (IIO, printError, splitPair, guessElement, pipeSymbols)
import Data.Char (isSpace)
import Action.Substitute.Bundle
import Action.Convert.Bundle
import Cache.Bundle (Cache, cache)
-- import Data.Char (isSpace)

data ActionTag = SIMPL | SUBS | SHOW | PRINT | CONV
  deriving (Show, Read, Eq, Enum, Bounded)

showActionTag :: ActionTag -> String
showActionTag SIMPL = "simplify"
showActionTag SUBS = "substitute"
showActionTag SHOW = "show"
showActionTag PRINT = "print"
showActionTag CONV = "convert"
-- showActionTag RETURN = "return"

allActionTags :: [ActionTag]
allActionTags = enumFrom minBound

vacuum :: String -> String
vacuum = filter (not . isSpace)

parseActionChain :: [ActionTag] -> String -> Result [ActionTag]
parseActionChain ats = fmap (uncurry (++)) . go ats . vacuum
  where
    go :: [ActionTag] -> String -> Result ([ActionTag], [ActionTag])
    go acc "" = Content (acc, [])
    go acc (s:str) =
      let (cur, rest) = splitPair True (`elem` pipeSymbols) str
       in case s of
            '$' -> go acc rest >>= \ (l,r) -> (\x -> (l, x:r)) <$> guessElement showActionTag cur allActionTags
            '#' -> guessElement showActionTag cur allActionTags >>= \at -> go (at : acc) rest
            '@' -> guessElement showActionTag cur allActionTags >>= \at -> go [at] rest
            _ -> Error $ "unrecognized pipe symbol: `" ++ [s] ++ "`"

instance Description ActionTag where description _ = "an action tag"

type AllActions :: ObjectTag -> Constraint
type family AllActions tg where
  AllActions tg = (
      Description tg,
      DisplayResult tg,
      ParseResult tg,
      Cache tg,
      SimplifyResult tg,
      SubstituteResult tg,
      ConvertResult tg
    )

parseByTag ::
  ObjectTag ->
  String ->
  (forall tg. AllActions tg => Result (Object tg) -> b) ->
  b
parseByTag IEX a cont = cont (parseResult a :: Result (Object IEX))
parseByTag REX a cont = cont (parseResult a :: Result (Object REX))
parseByTag STR a cont = cont (parseResult a :: Result (Object STR))
parseByTag BEX a cont = cont (parseResult a :: Result (Object BEX))

addAns :: Cache tg => [String] -> Result (Object tg) -> [String]
addAns hist (Content obj) = (cache obj) : hist
addAns hist _ = hist

action :: AllActions tg =>
  ActionTag ->
  (forall tg'. AllActions tg' => Result (Object tg') -> [String] -> IIO a) ->
  Object tg ->
  [String] ->
  IIO a
action SIMPL cont obj hist = simplifyResult obj >>= raise displayResult >>= cont <*> (addAns hist)
action SUBS cont obj hist = substituteResult obj >>= raise displayResult >>= cont <*> (addAns hist)
action SHOW cont obj hist = displayResult (Raw (show obj)) >> cont (Content obj) (cache obj : hist)
action PRINT cont obj hist = displayResult obj >>= cont <*> (addAns hist)
action CONV cont obj hist = convertResult obj >>= raise displayResult >>= cont <*> (addAns hist)
-- action RETURN cont obj hist = cont (Content obj) (cache obj : hist)

actionChain :: AllActions tg =>
  [ActionTag] ->
  ([String] -> IIO ()) ->
  Object tg ->
  [String] ->
  IIO ()
actionChain [] cont _ hist = cont hist
actionChain (at:rest) cont obj hist = action at f obj hist
  where
    f :: forall tg'. AllActions tg' => Result (Object tg') -> [String] -> IIO ()
    f (Error msg) = (printError msg >>) . cont
    f (Content newobj) = actionChain rest cont newobj
