module Action.Simplify.Instances.BEX (simplifyResult) where

import Object.Bundle
import Result
import Control.Monad ((<=<))
import Data.List (sortBy)
import Data.Ord (comparing)

simplifyResult :: Object BEX -> Result (Object BEX)
simplifyResult = preEval <=< rearrange

rearrange :: Object BEX -> Result (Object BEX)
rearrange (BConst v) = Content (BConst v)
rearrange (BVar name) = Content (BVar name)
rearrange (BNot e) = BNot <$> simplifyResult e
rearrange (BAnd e1 e2) = do
  es <- conjuncts (BAnd e1 e2)
  Content $ foldr1 BAnd $ sortBy (comparing operandKey) es
rearrange (BOr e1 e2) = do
  es <- disjuncts (BOr e1 e2)
  Content $ foldr1 BOr $ sortBy (comparing operandKey) es
rearrange (BXor e1 e2) = do
  es <- xors (BXor e1 e2)
  Content $ foldr1 BXor $ sortBy (comparing operandKey) es
rearrange (BImpl e1 e2) = BImpl <$> simplifyResult e1 <*> simplifyResult e2
rearrange (BEquiv e1 e2) = BEquiv <$> simplifyResult e1 <*> simplifyResult e2

conjuncts :: Object BEX -> Result [Object BEX]
conjuncts (BAnd e1 e2) = (++) <$> conjuncts e1 <*> conjuncts e2
conjuncts e = (: []) <$> simplifyResult e

disjuncts :: Object BEX -> Result [Object BEX]
disjuncts (BOr e1 e2) = (++) <$> disjuncts e1 <*> disjuncts e2
disjuncts e = (: []) <$> simplifyResult e

xors :: Object BEX -> Result [Object BEX]
xors (BXor e1 e2) = (++) <$> xors e1 <*> xors e2
xors e = (: []) <$> simplifyResult e

operandKey :: Object BEX -> (Int, String)
operandKey (BConst True) = (0, "1")
operandKey (BConst False) = (0, "0")
operandKey (BVar name) = (1, name)
operandKey (BNot (BVar name)) = (1, '¬' : name)
operandKey (BNot e) = (2, show e)
operandKey _ = (2, "")

isTrue :: Object BEX -> Bool
isTrue (BConst True) = True
isTrue _ = False

isFalse :: Object BEX -> Bool
isFalse (BConst False) = True
isFalse _ = False

preEval :: Object BEX -> Result (Object BEX)
preEval (BConst v) = Content (BConst v)
preEval (BVar name) = Content (BVar name)
preEval (BNot (BNot e)) = preEval e
preEval (BNot e) = do
  e' <- preEval e
  case e' of
    BConst True -> Content (BConst False)
    BConst False -> Content (BConst True)
    e'' -> Content (BNot e'')
preEval (BAnd e1 e2) = do
  f1 <- preEval e1
  f2 <- preEval e2
  case f2 of
    BAnd f2' f3 -> do
      g1 <- combineAnd f1 f2'
      case g1 of
        BAnd g2 g3 -> Content (BAnd g2 (BAnd g3 f3))
        BConst False -> Content (BConst False)
        g1' -> combineAnd g1' f3
    _ -> combineAnd f1 f2
preEval (BOr e1 e2) = do
  f1 <- preEval e1
  f2 <- preEval e2
  case f2 of
    BOr f2' f3 -> do
      g1 <- combineOr f1 f2'
      case g1 of
        BOr g2 g3 -> Content (BOr g2 (BOr g3 f3))
        BConst True -> Content (BConst True)
        g1' -> combineOr g1' f3
    _ -> combineOr f1 f2
preEval (BXor e1 e2) = do
  f1 <- preEval e1
  f2 <- preEval e2
  case f2 of
    BXor f2' f3 -> do
      g1 <- combineXor f1 f2'
      case g1 of
        BXor g2 g3 -> Content (BXor g2 (BXor g3 f3))
        g1' -> combineXor g1' f3
    _ -> combineXor f1 f2
preEval (BImpl e1 e2) = do
  f1 <- preEval e1
  f2 <- preEval e2
  case (f1, f2) of
    (BConst False, _) -> Content (BConst True)
    (_, BConst True) -> Content (BConst True)
    (BConst True, e) -> Content e
    (e, BConst False) -> preEval (BNot e)
    (e1', e2') | e1' == e2' -> Content (BConst True)
    (BNot a, b) | a == b -> Content (BConst True)
    (a, BNot b) | a == b -> Content (BConst True)
    _ -> Content (BOr (BNot f1) f2)
preEval (BEquiv e1 e2) = do
  f1 <- preEval e1
  f2 <- preEval e2
  case (f1, f2) of
    (BConst b1, BConst b2) -> Content (BConst (b1 == b2))
    (BConst True, e) -> Content e
    (e, BConst True) -> Content e
    (BConst False, e) -> preEval (BNot e)
    (e, BConst False) -> preEval (BNot e)
    (e1', e2') | e1' == e2' -> Content (BConst True)
    (BNot a, b) | a == b -> Content (BConst False)
    (a, BNot b) | a == b -> Content (BConst False)
    _ -> Content (BAnd (BOr (BNot f1) f2) (BOr f1 (BNot f2)))

combineAnd :: Object BEX -> Object BEX -> Result (Object BEX)
combineAnd e1 e2
  | isFalse e1 || isFalse e2 = Content (BConst False)
  | isTrue e1 = Content e2
  | isTrue e2 = Content e1
  | e1 == e2 = Content e1
  | otherwise = case (e1, e2) of
      (BNot a, b) | a == b -> Content (BConst False)
      (a, BNot b) | a == b -> Content (BConst False)
      _ -> Content (BAnd e1 e2)

combineOr :: Object BEX -> Object BEX -> Result (Object BEX)
combineOr e1 e2
  | isTrue e1 || isTrue e2 = Content (BConst True)
  | isFalse e1 = Content e2
  | isFalse e2 = Content e1
  | e1 == e2 = Content e1
  | otherwise = case (e1, e2) of
      (BNot a, b) | a == b -> Content (BConst True)
      (a, BNot b) | a == b -> Content (BConst True)
      _ -> Content (BOr e1 e2)

combineXor :: Object BEX -> Object BEX -> Result (Object BEX)
combineXor e1 e2
  | isFalse e1 = Content e2
  | isFalse e2 = Content e1
  | isTrue e1 = do
      e2' <- preEval (BNot e2)
      Content e2'
  | isTrue e2 = do
      e1' <- preEval (BNot e1)
      Content e1'
  | e1 == e2 = Content (BConst False)
  | otherwise = case (e1, e2) of
      (BNot a, b) | a == b -> Content (BConst True)
      (a, BNot b) | a == b -> Content (BConst True)
      _ -> Content (BXor e1 e2)
