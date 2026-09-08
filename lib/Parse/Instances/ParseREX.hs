module Parse.Instances.ParseREX (parse) where
import Result
import Object.Bundle
import Object.RealNumber
import Data.List (elemIndex)
import Data.Ratio

mkError :: String -> Result a
mkError msg = Error ("could not parse real expression: " ++ msg)

parse :: String -> Result (Object REX)
parse = todo

parseRN :: String -> Result RealNumber
parseRN = todo

parseRC :: String -> Result RealConst 
parseRC [] = mkError "expected a constant"
parseRC "e" = Content E
parseRC "pi" = Content PI
parseRC "gamma" = Content GAMMA
parseRC "ln(2)" = Content LN2
parseRC str = case elemIndex '\\' str of
  Nothing -> Rw <$> readResult str
  Just i -> let (s1,s2) = splitAt i str
             in fmap Rt $ (%) <$> readResult s1 <*> readResult s2
