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
      rows = map (fmap $ renderSingle (-1)) symsSplitSorted
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

smin :: [Symbol] -> (Int, Int)
smin syms = (minimum $ map (select 1) syms, minimum $ map (select 2) syms)

smax :: [Symbol] -> (Int, Int)
smax syms = (maximum $ map (select 1) syms, maximum $ map (select 2) syms)

placeString :: Int -> String -> [Symbol]
placeString y = zipWith (\x c -> (x, y, c)) [0..]

place :: Int -> Int -> [Symbol] -> [Symbol]
place x y syms =
  let (mx, my) = smin syms
   in shift (x - mx) (y - my) syms

binop :: Char -> [Symbol] -> [Symbol] -> [Symbol]
binop c s1 s2 = 
  let (mx, _) = smax s1
   in s1 ++ [(mx+2, 0, c)] ++ shift (mx+4) 0 s2
