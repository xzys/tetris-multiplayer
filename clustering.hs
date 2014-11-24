import qualified Data.ByteString.Char8 as B
import Debug.Trace
import Data.List as L
import Data.Set as S

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
{-
simMatrix c p el = copyOver [[ if j > i then distance c p (el !! i) (el !! j) else 0 | j <- eindex] | i <- eindex]
    where   eindex = [0..length el - 1]
-}
simMatrix c p el = [[0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 10.0, 0.0, 0.0, 0.0], [0.0, 10.0, 0.0, 10.0, 0.0, 10.0], [0.0, 0.0, 10.0, 0.0, 5.0, 0.0], [0.0, 0.0, 0.0, 5.0, 0.0, 0.0],[0.0, 0.0, 10.0, 0.0, 0.0, 0.0]]

copyOver :: [[Double]] -> [[Double]]
copyOver sm = [[ if j < i then sm !! j !! i else sm !! i !! j | j <- eindex] | i <- eindex]
    where   eindex = [0..length sm - 1]


-- find subtree from a starting point
-- frontier + list of nodes to go to
{-
searchTree visit d sm found acc | trace ("search: " ++ show visit ++ " " ++ show found) False = undefined
-}
searchTree :: [Int] -> Double -> [[Double]] -> [Int] -> [[Int]] -> [[Int]]
searchTree (s:visit) d sm found acc = 
    let eindex = [0..length sm - 1]
        -- so has to be connected and not found already, or already a part of visit
        nvisit = L.filter (\x -> (d < sm !! s !! x && x `notElem` found) || x `elem` visit) eindex
    in  if length nvisit == 0 then 
            if compare1d2d (s:found) acc then acc     -- return condition
            else (s:found):acc                        -- add this result onto acc
        else searchTree nvisit d sm (s:found) acc     -- go to next node

{- 
allConnected d sm = [searchTree [s] d sm [] | s <- eindex]
-}
-- find all subtrees
allConnected :: Double -> [[Double]] -> [[Int]]
allConnected d sm = L.foldl (\acc x -> searchTree [x] d sm [] acc) [] eindex
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
		in	if compare2d2d (head acc) clusters then acc -- discard if same cluster
				else clusters:acc -- we are adding on to the head here
	in 	init $ L.foldl diff [[]] [0,2..10]

distance :: DBC -> Property -> Entity -> Entity -> Double
distance c p e1 e2 = 5





{-
compare1d2d x (y:ys) | trace ("1d: " ++ show x ++ " " ++ show y) False = undefined
compare1d2d x (y:ys) = null (x \\ y) || compare1d2d x ys
-}
-- should give false if they are not equal, true if equal
compare1d2d :: (Eq a, Show a) => [a] -> [[a]] -> Bool
compare1d2d _ [] = False
compare1d2d x (y:ys) = L.null (x L.\\ y) || compare1d2d x ys

{- 
compare2d2d xs [] = False 
compare2d2d xs (y:ys) = compare1d2d y xs || compare2d2d xs ys
compare2d2d xs ys | trace ("compare: " ++ show xs ++ " " show ys) False = undefined
-}
compare2d2d :: (Ord a, Eq a, Show a) => [[a]] -> [[a]] -> Bool
compare2d2d xs ys = set2d xs == set2d ys
    where set2d list = S.fromList [S.fromList l | l <- list]