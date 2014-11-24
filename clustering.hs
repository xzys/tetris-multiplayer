import qualified Data.ByteString.Char8 as B
import Debug.Trace
import Data.List

type Entity = [Char]
type Property = Int
type DBC = Int


{-
simMatrix c p el = [[distance c p e1 e2 | e1 <- el] | e2 <- el]
-}
-- create similarity matrix
-- this version copies over fold so you don't have to do distance twice
-- it's kind of ugly
simMatrix :: DBC -> Property -> [Entity] -> [[Double]]
simMatrix c p el = copyOver [[ if j > i then distance c p (el !! i) (el !! j) else 0 | j <- eindex] | i <- eindex]
    where   eindex = [0..length el - 1]

copyOver :: [[Double]] -> [[Double]]
copyOver sm = [[ if j < i then sm !! j !! i else sm !! i !! j | j <- eindex] | i <- eindex]
    where   eindex = [0..length sm - 1]


-- find subtree from a starting point
-- frontier + list of nodes to go to
searchTree :: [Int] -> Double -> [[Double]] -> [Int] -> [[Int]] -> Int -> [[Int]]
searchTree visit d sm found acc 1 | trace ("search: " ++ show visit ++ " " ++ show found) False = undefined
searchTree (s:visit) d sm found acc n = 
    let eindex = [0..length sm - 1]
        -- so has to be connected and not found already, or already a part of visit
        nvisit = filter (\x -> (d < sm !! s !! x && x `notElem` found) || x `elem` visit) eindex
    in  if length nvisit == 0 then 
            if alreadyFound (s:found) acc then acc -- return condition
            else (s:found):acc                        -- add this result onto acc
        else searchTree nvisit d sm (s:found) acc 0   -- go to next node

alreadyFound :: (Eq a) => [a] -> [[a]] -> Bool
alreadyFound x [] = False
alreadyFound x (y:ys) = null (x \\ y) || alreadyFound x ys

{- 
allConnected d sm = [searchTree [s] d sm [] | s <- eindex]
-}
-- find all subtrees
allConnected :: Double -> [[Double]] -> [[Int]]
allConnected d sm = foldl (\acc x -> searchTree [x] d sm [] acc 0) [] eindex
    where eindex = [0..length sm - 1]

{-
thresholdClustering c p el = [allConnected d (simMatrix c p el) | d <- [10, 20..500]]
-}
-- search tree for each d value
-- toss away clusterings that are the same
thresholdClustering :: DBC -> Property -> [Entity] -> [[[Int]]]
thresholdClustering c p el = 
	let diff acc d = 
		let clusters = allConnected d (simMatrix c p el)
		in	if head acc == clusters then acc 	-- head because 
				else clusters:acc 				-- we are adding on to the head here
	in 	foldl diff [[[]]] [10, 20..500]

distance :: DBC -> Property -> Entity -> Entity -> Double
distance c p e1 e2 = 5