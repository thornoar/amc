module Action.Display.Instances.BEX (display) where
import Object.Bundle
import Action.Display.Render

display :: Object BEX -> String
display = render . assemble

addParensFor :: Int -> Object BEX -> [Symbol] -> [Symbol]
addParensFor p (BAnd _ _)
  | p > 0 = addParens
addParensFor p (BOr _ _)
  | p > 0 = addParens
addParensFor p (BXor _ _)
  | p > 0 = addParens
addParensFor p (BImpl _ _)
  | p > 0 = addParens
addParensFor p (BEquiv _ _)
  | p > 0 = addParens
addParensFor _ _ = id

assemble :: Object BEX -> [Symbol]
assemble (BConst v) = placeString 0 0 (if v then "1" else "0")
assemble (BVar name) = placeString 0 0 name
assemble (BNot obj) = (0,0,'¬') : addParensFor 1 obj (shift 1 0 $ assemble obj)
assemble (BAnd o1 o2) = binop "⋀" (addParensFor 1 o1 $ assemble o1) (addParensFor 1 o2 $ assemble o2)
assemble (BOr o1 o2) = binop "⋁" (addParensFor 1 o1 $ assemble o1) (addParensFor 1 o2 $ assemble o2)
assemble (BXor o1 o2) = binop "⊕" (addParensFor 1 o1 $ assemble o1) (addParensFor 1 o2 $ assemble o2)
assemble (BImpl o1 o2) = binop "=>" (addParensFor 1 o1 $ assemble o1) (addParensFor 1 o2 $ assemble o2)
assemble (BEquiv o1 o2) = binop "<=>" (addParensFor 1 o1 $ assemble o1) (addParensFor 1 o2 $ assemble o2)
