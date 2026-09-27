module Display.Instances.DisplayREX (display) where
import Object.Bundle
import Display.Render
import Object.RealNumber

display :: Object REX -> String
display = render . assemble

assembleRN :: RealNumber -> [Symbol]
assembleRN (In v) = placeString 0 (show v)
assembleRN (Dbl v) = placeString 0 (show v)
assembleRN PI = placeString 0 "π"
assembleRN E = [(0,0,'e')]
assembleRN GAMMA = placeString 0 "γ"
assembleRN LN2 = placeString 0 "ln(2)"

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
      (minx1, _) = smin s1
      (minx2, miny2) = smin s2
      (maxx1, _) = smax s1
      (maxx2, maxy2) = smax s2
      l1 = maxx1 - minx1 + 1
      l2 = maxx2 - minx2 + 1
      h2 = maxy2 - miny2 + 1
      lmax = max l1 l2
   in place ((lmax - l1) `div` 2) h2 s1
      ++ placeString 0 (replicate lmax '-')
      ++ place ((lmax - l2) `div` 2) (-h2) s2
assemble (RLog o1 o2) =
  let s1 = assemble o1
      s2 = assemble o2
      (minx1, miny1) = smin s1
      (maxx1, maxy1) = smax s1
      l1 = maxx1 - minx1 + 1
      h1 = maxy1 - miny1 + 1
   in placeString 0 "log" ++ place 3 (-h1) s1 ++ shift (4 + l1) 0 s2
assemble (RPow o1 o2) = binop '^' (assemble o1) (assemble o2)
assemble (RVar name) = placeString 0 name
assemble (RApp fun obj) = placeString 0 (show fun) ++ shift 5 0 (assemble obj)
