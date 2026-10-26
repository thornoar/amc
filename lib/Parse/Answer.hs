{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parse.Answer where
import Object.Bundle
import Result
import Data.Kind (Constraint)

type SubsAnswer :: ObjectTag -> ObjectTag -> Constraint
class SubsAnswer tg1 tg2 where
  subsAnswer :: Object tg1 -> Object tg2 -> Result (Object tg2)

-- instance {-# OVERLAPPABLE #-} (Description tg1, Description tg2) => SubsAnswer tg1 tg2 where
--   subsAnswer _ _ = Error 
--
-- instance SubsAnswer STR where
--   subsAnswer _ ans (Raw str) = Content . Raw $ replaceAns (show ans) str
--
-- -- instance SubsAnswer IEX where
-- --   subsAnswer IEX new obj = 
--
-- replaceAns :: String -> String -> String
-- replaceAns new ('a':'n':'s':rest) = new ++ replaceAns new rest
-- replaceAns _ "" = ""
-- replaceAns new (a:rest) = a : replaceAns new rest
