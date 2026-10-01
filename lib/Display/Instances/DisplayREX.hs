module Display.Instances.DisplayREX (display) where
import Object.Bundle
import Display.Render
import Object.RealNumber
import Data.Char (toLower)

display :: Object REX -> String
display = render . assemble

assembleRN :: RealNumber -> [Symbol]
assembleRN (In v) = placeString 0 (show v)
assembleRN (Dbl v) = placeString 0 (show v)
assembleRN PI = placeString 0 "π"
assembleRN E = [(0,0,'e')]
assembleRN GAMMA = placeString 0 "γ"
assembleRN LN2 = placeString 0 "ln(2)"

addParensFor :: Int -> Object REX -> [Symbol] -> [Symbol]
addParensFor p (RSum _ _)
  | p > 0 = addParens
addParensFor p (RDiff _ _)
  | p > 0 = addParens
addParensFor p (RProd _ _ )
  | p > 1 = addParens
addParensFor p (RDiv _ _ )
  | p > 2 = addParens
addParensFor p (RPow _ _ )
  | p > 3 = addParens
addParensFor p _
  | p > 50 = addParens
addParensFor _ _ = id

assemble :: Object REX -> [Symbol]
assemble (RConst v) = assembleRN v
assemble (RNeg obj) = (0,0,'-') : shift 1 0 (assemble obj)
assemble (RSum o1 o2) = binop '+' (assemble o1) (assemble o2)
assemble (RDiff o1 o2) = binop '-' (assemble o1) (assemble o2)
assemble (RInv obj) = assemble (RDiv (RConst (In 1)) obj)
assemble (RProd o1 o2) = binop '*' (assemble o1) (assemble o2)
assemble (RDiv o1 o2) =
  let s1 = assemble o1
      s2 = assemble o2
      (min1x, _) = smin' s1
      (min2x, min2y) = smin' s2
      (max1x, _) = smax' s1
      (max2x, max2y) = smax' s2
      l1 = max1x - min1x + 1
      l2 = max2x - min2x + 1
      h2 = max2y - min2y + 1
      -- lmax = maximum $ [3, l1, l2]
      lmax = 2 + max l1 l2
   in place ((lmax - l1) `div` 2) 1 s1
      ++ placeString 0 (replicate lmax '⎼') -- '⎼' '─'
      ++ place ((lmax - l2) `div` 2) (-h2) s2
assemble (RLog o1 o2) =
  let s1 = addParensFor 45 o1 $ assemble o1
      s2 = addParensFor 45 o2 $ assemble o2
      (min1x, min1y) = smin' s1
      (max1x, max1y) = smax s1
   in placeString 0 "log" ++
      place 3 (min1y - max1y - 1) s1 ++ shift (5 + max1x - min1x) 0 s2
assemble (RPow o1 o2) =
  let (s1, s2) = (addParensFor 40 o1 $ assemble o1, assemble o2)
      (max1x, max1y) = smax s1
   in s1 ++ place (max1x+1) (max1y+1) s2
assemble (RVar name) = placeString 0 name
assemble (RApp fun obj) =
  let name = map toLower $ show fun
   in placeString 0 name ++ shift (length name) 0 (addParensFor 100 obj $ assemble obj)
