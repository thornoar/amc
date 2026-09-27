module Display.Instances.DisplayIEX where
import Object.Bundle
import Display.Render

display :: Object IEX -> String
display = render . assemble

assemble :: Object IEX -> [Symbol]
assemble (IConst v) = placeString 0 (show v)
assemble (INeg obj) = (0,0,'-') : shift 1 0 (assemble obj)
assemble (ISum o1 o2) = binop '+' (assemble o1) (assemble o2)
assemble (IDiff o1 o2) = binop '-' (assemble o1) (assemble o2)
assemble (IProd o1 o2) = binop '*' (assemble o1) (assemble o2)
assemble (IDiv o1 o2) =
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
assemble (IMod o1 o2) = binop '%' (assemble o1) (assemble o2)
assemble (IPow o1 o2) = binop '^' (assemble o1) (assemble o2)
assemble (IVar name) = placeString 0 name
