module Display.Instances.DisplayIEX where
import Object.Bundle
import Display.Render

display :: Object IEX -> String
display = render . assemble

addParensFor :: Int -> Object IEX -> [Symbol] -> [Symbol]
addParensFor p (ISum _ _)
  | p > 0 = addParens
addParensFor p (IDiff _ _)
  | p > 0 = addParens
addParensFor p (IProd _ _ )
  | p > 1 = addParens
addParensFor p (IDiv _ _ )
  | p > 2 = addParens
addParensFor p (IMod _ _ )
  | p > 3 = addParens
addParensFor p (IPow _ _ )
  | p > 4 = addParens
addParensFor _ _ = id

assemble :: Object IEX -> [Symbol]
assemble (IConst v) = placeString 0 (show v)
assemble (INeg obj) = addParensFor 1 obj $ (-1,0,'-') : assemble obj
assemble (ISum o1 o2) = binop '+' (assemble o1) (assemble o2)
assemble (IDiff o1 o2) = binop '-' (assemble o1) (assemble o2)
assemble (IProd o1 o2) = binop '*' (addParensFor 1 o1 $ assemble o1) (addParensFor 1 o2 $ assemble o2)
assemble (IDiv o1 o2) =
  let s1 = assemble o1
      s2 = assemble o2
      (min1x, _) = smin s1
      (min2x, min2y) = smin s2
      (max1x, _) = smax s1
      (max2x, max2y) = smax s2
      l1 = max1x - min1x + 1
      l2 = max2x - min2x + 1
      h2 = max2y - min2y + 1
      lmax = maximum [3, l1, l2]
   in place ((lmax - l1) `div` 2) 1 s1
      ++ placeString 0 (replicate lmax '-')
      ++ place ((lmax - l2) `div` 2) (-h2) s2
assemble (IMod o1 o2) = binop '%' (addParensFor 2 o1 $ assemble o1) (addParensFor 2 o2 $ assemble o2)
assemble (IPow o1 o2) =
  let (s1, s2) = (addParensFor 100 o1 $ assemble o1, assemble o2)
      (max1x, max1y) = smax s1
   in s1 ++ place (max1x+1) (max1y+1) s2
assemble (IVar name) = placeString 0 name
