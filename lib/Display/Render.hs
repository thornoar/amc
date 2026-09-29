module Display.Render where
import Data.List (partition, sortBy)
import Data.Ord (comparing, Down (..))

type Symbol = (Int, Int, Char)

select :: Int -> (a,a,b) -> a
select 1 (a,_,_) = a
select _ (_,b,_) = b

render :: [Symbol] -> String
render syms =
  let symsSplit = splitList (select 2) syms
      symsSplitSorted = sortBy (comparing (Down . fst)) symsSplit
      rows = map (fmap $ renderSingle (-1) . sortBy (comparing $ select 1)) symsSplitSorted
   in case rows of
        [] -> ""
        ((y,_):_) -> "\n" ++ makeBlock y rows

splitList :: Eq b => (a -> b) -> [a] -> [(b, [a])]
splitList _ [] = []
splitList sel (a:as) =
  let b = sel a
      (l, r) = partition ((== b) . sel) as
   in (b, a:l) : splitList sel r

makeBlock :: Int -> [(Int, String)] -> String
makeBlock _ [] = ""
makeBlock pad ((y, str):rest) = replicate (pad - y - 1) '\n' ++ str ++ makeBlock y rest

renderSingle :: Int -> [Symbol] -> String
renderSingle _ [] = "\n"
renderSingle pref ((x,_,c):rest) = replicate (x - pref) ' ' ++ [c] ++ renderSingle (x+1) rest

shift :: Int -> Int -> [Symbol] -> [Symbol]
shift x y = map $ \ (x',y',c) -> (x'+x, y'+y, c)

-- smin :: [Symbol] -> (Int, Int)
-- smin syms = (minimum $ map (select 1) syms, minimum $ map (select 2) syms)

extreme :: (Int -> Int -> Bool) -> [Symbol] -> (Int, Int)
extreme _ [] = (0,0)
extreme cmp ((sx,sy,_):rest) = go (sx,sy) rest
  where
    go :: (Int, Int) -> [Symbol] -> (Int, Int)
    go p [] = p
    go (x,y) ((x',y',_):syms)
      | cmp y' y = go (x,y) syms
      | cmp y y' = go (x',y') syms
      | cmp x x' = go (x',y') syms
      | otherwise = go (x,y) syms

smin :: [Symbol] -> (Int, Int)
smin = extreme (>)

smax :: [Symbol] -> (Int, Int)
smax = extreme (<)
-- smax syms = (maximum $ map (select 1) syms, maximum $ map (select 2) syms)

placeString :: Int -> String -> [Symbol]
placeString y = zipWith (\x c -> (x, y, c)) [0..]

place :: Int -> Int -> [Symbol] -> [Symbol]
place x y syms =
  let (mx, my) = smin syms
   in shift (x - mx) (y - my) syms

binop :: Char -> [Symbol] -> [Symbol] -> [Symbol]
binop c s1 s2 = 
  let (m1x, _) = smax s1
   in s1 ++ [(m1x+2, 0, c)] ++ shift (m1x + 4) 0 s2

addParens :: [Symbol] -> [Symbol]
addParens syms =
  let (ax, ay) = smax syms
      (_, iy) = smin syms
   in case ay - iy of
        0 -> (0, iy, '(') : (ax + 2, ay, ')') : shift 1 0 syms
        _ ->
          let lft = (0, iy, '\\') : (0, ay, '/') : map (\y -> (0, y, '⎸')) [(iy + 1) .. (ay - 1)]
              rgt = (ax+2, iy, '/') : (ax+2, ay, '\\') : map (\y -> (ax+3, y, '⎸')) [(iy + 1) .. (ay - 1)] -- '⎸' 
           in lft ++ shift 1 0 syms ++ rgt
