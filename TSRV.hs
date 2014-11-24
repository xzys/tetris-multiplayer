-- RHINE Hackathon RC
-- (c) Speare 2014

{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings #-}

import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Search as BS
import qualified System.ZMQ4.Monadic as Z
import Data.Maybe (fromMaybe)
import Data.List (sortBy, tails)
import Data.Double.Conversion.ByteString (toFixed)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad (forever, replicateM_)
import System.Timeout (timeout)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle, hClose)
import Control.Concurrent (forkIO)
import Data.Char (isSpace)
import Math.Statistics (stddev)

import qualified THAux
import qualified Aux

port = 81

--main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber port
    putStrLn $ "Server started on port " ++ show port
    conn <- getConn
    sockHandler sock conn

sockHandler sock c = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ do
	res <- timeout 5000000000 $ process handle c
	case res of
	    Just a -> return ()
	    Nothing -> hPutStrLn handle "{\"error\": \"Timeout - please try again.\"}" >> hClose handle
    sockHandler sock c

process handle c = do
    line <- hGetLine handle
    putStrLn $ show line
    reply <- parse line c
    putStrLn $ show $ reply
    hPutStrLn handle $ reply
    hClose handle

parse line c =
    case method of 
	"distance" -> do
   	    putStrLn $ show route
	    e1 <- ep c p1
	    e2 <- ep c p2
	    res <- run' c "semantic" "distance" $ B.concat [e1, "#", e2]
	    return $ B.unpack $ B.concat ["{\"distance\": ", res, "}"]
	"closest_entities" -> do
	    e <- ep' c p1
            res <- run' c "semantic" "closestEntities" e
	    return $ B.unpack $ B.concat ["{\"closest_entities\": ", res, "}"]
	"synonym_check" -> do
	    e1 <- ep' c p1
	    e2 <- ep' c p2
	    res <- run' c "semantic" "synonymCheck" $ B.concat [e1, "#", e2]
	    return $ B.unpack $ B.concat ["{\"synonym\": \"", res, "\"}"]
	"entity_extraction" -> do
	    res <- ee p1
	    return $ B.unpack $ B.concat ["{\"entities\": ", res, "}"]
	"best_match" -> do
	    tomatch <- ep c p1
	    poss <- ep'' c p2
	    res <- run' c "semantic" "bestMatch" $ B.concat [tomatch, "#", poss, "#", (B.pack . show) 1]
	    return $ B.unpack $ B.concat ["{\"best_match\": ", res, "}"]
	_ -> return "{\"error\": \"Invalid method.\"}"
    where route = let x = (snd $ B.breakSubstring "GET " $ fst $ B.breakSubstring " HTTP" $ B.pack line) in x--toStrict $ BS.replace " " ("%20" :: B.ByteString) x
	  (_, apikey, method, p1, p2, p3) = $(THAux.recBreak 6) route '/'

ee text = Z.runZMQ $ do
   s <- Z.socket Z.Req
   Z.connect s "tcp://127.0.0.1:6667"
   Z.send s [] text
   Z.receive s

ep'' c e = do
   sub <- sequence $ map (ep c) $ map (B.drop 1) $ B.split ')' $ B.filter (/='(') $ B.take (B.length e - 1) e
   return $ B.concat ["[", B.intercalate "," sub, "]"]

ep' c e = do
   conv <- el c e
   putStrLn $ show (show e, show conv)
   return $ conv --B.concat ["\"", conv, "\""]

ep c e = do
   convs <- sequence $ map (el c) $ B.split ',' e
   putStrLn $ show (show e, show convs)
   return $ B.concat ["[", B.intercalate "," (map (\x -> B.concat ["\"", x, "\""]) convs), "]"]

el c e = Z.runZMQ $ do
--    return e
    s <- Z.socket Z.Req
    Z.connect s "tcp://127.0.0.1:6666"
    Z.send s [] e
    Z.receive s
--   cached <- fmap (fromMaybe "null" . fromEither "null") $ R.runRedis c $ R.get e
--   if cached != "null" then return cached else do
	
	
{-
-- ZMQ Stack

numWorkers = 20

main :: IO ()
main = Z.runZMQ $ do
    server <- Z.socket Z.Router
    Z.bind server "tcp://*:9999"
--    Z.bind server "tcp://127.0.0.1:9999"
  
    workers <- Z.socket Z.Dealer
    Z.bind workers "inproc://workers"
    
    -- Fork the workers.
    replicateM_ numWorkers $ Z.async worker

    -- Connect workers via queue.
    Z.proxy server workers Nothing

worker :: Z.ZMQ z ()
worker = do
    receiver <- Z.socket Z.Rep
    Z.connect receiver "inproc://workers"
    
    conn <- getConn    
    
    forever $ do
        recv <- Z.receive receiver
        putStrLn $ (++) "RECV: " $ B.unpack recv
        result <- fmap (fromMaybe "timeout") $ timeout 1000000 $ let (metric, method, params) = $(THAux.recBreak 3) recv '&' in run' conn metric method params
        putStrLn $ (++) "SENT: " $ B.unpack result
        Z.send receiver [] result
-}

-- ZMQ Translation

run' :: DBC -> B.ByteString -> B.ByteString -> B.ByteString -> IO B.ByteString
run' _ "" _ _ = return "\"Invalid metric.\""
run' _ _ "" _ = return "\"Invalid method.\""
run' _ _ _ "" = return "\"Invalid params.\""
run' c "semantic" method params = run c wikilinks method params
run' c "news" method params = run c newslinks method params
run' _ _ _ _ = return "Invalid metric."
        
run :: DBC -> Property -> B.ByteString -> B.ByteString -> IO B.ByteString
run c p "distance" args = fmap parseDouble $ ndistance c p e1 e2
    where (e1, e2) = (\(x, y) -> (Aux.parseToBSL x, Aux.parseToBSL y)) $ $(THAux.recBreak 2) args '#'
run c p "bestMatch" args = fmap (B.pack . show) $ bestMatch c e1 e2 n
    where (e1, e2, n) = (\(x, y, z) -> (Aux.parseToBSL x, Aux.parseToBSLL y, Aux.parseToInt z)) $ $(THAux.recBreak 3) args '#'
run c p "closestEntities" args = fmap (B.pack . show) $ closestEntities c p e
    where e = Aux.parseToBS $ $(THAux.recBreak 1) args '#'
--run c p "superCategory" args = fmap (B.pack . show) $ superCategory c e
--    where e = Aux.parseToBS $ $(THAux.recBreak 1) args '#'
run c p "synonymCheck" args = fmap (B.pack . show) $ synonymCheck c e1 e2
    where (e1, e2) = (\(x, y) -> (Aux.parseToBS x, Aux.parseToBS y)) $ $(THAux.recBreak 2) args '#'
run c p "clustering" args = do fmap (B.pack . show) $ clustering c e
    where e = Aux.parseToBSL $ $(THAux.recBreak 1) args '#'





-- sach trying things right now
run c p "dClustering" args = do fmap (B.pack . show) $ clustering c e
    where e = Aux.parseToBSL $ $(THAux.recBreak 1) args '#'

-- create similarity matrix
simatrix :: [Entity] -> [[Double]]
simatrix el = [[distance c p el e2 | e1 <- el] | e2 <- el]

simatrix el = copyOver [[ if j > i then distance c p (el !! i) (el !! j) else 0 | j <- eindex] | i <- eindex]
    where eindex = [0..length el - 1]
-- copy over fold so you don't have to do distance twice
copyOver :: [[Double]] -> [[Double]]
copyOver sm = [[ if j < i then sm !! j !! i else sm !! i !! j | j <- eindex | i <- eindex]
    where eindex = [0..length sm - 1]

-- search all d values
searchTree visit d sm found = 


searchTree :: Int -> Double -> [[Double] -> [Int] -> [Int]

searchTree s d sm found = 
    let visit = filter (\e d > sm !! s !! e && notElem e found) eindex -- nodes to go to

    in  if length visit == 0 then found else
        if searchTree (head visit) d sm (s:found)

    where eindex = [0..length sm - 1]


dClustering :: Double -> [[Double]] -> [Int] -> [[Int]]
dClustering d sm = [[   ] | sm <- sm, ]




-- end trying things and back to working things






run _ _ _ _ = return $ "Invalid method."

parseDouble :: Double -> B.ByteString
parseDouble = toFixed 8

readDouble :: B.ByteString -> Double
readDouble = read . B.unpack


-- Configuration

distanceThreshold = 25.0
distanceThreshold' = 60.0

--getConn = R.connect R.defaultConnectInfo
getConn = R.connect $ R.defaultConnectInfo { R.connectPort = R.UnixSocket "/var/run/redis/redis.sock" }

-- Algorithms

cLF = 2

clustering c es = fmap (filter (\(_, d) -> d < distanceThreshold) . sortBy (\(_, d1) (_, d2) -> compare d1 d2)) $ fmap (zip clusters) $ sequence $ map (cdist c) clusters
    where cdist c l = fmap sum $ sequence $ (map (\sl -> fmap (\x -> x / fromIntegral (length sl ^ cLF)) $ interDistance c sl) l)
--    where cdist c l = fmap sum $ sequence $ (map (\sl -> ((\x -> x / (if length sl == 1 then return 1.0 else distf)) =<< interDistance c sl)) l)
--	  distf = interDistance c es
          clusters = uniqueGroupings es

--clustering c es = fmap (filter (\(_, d) -> d < distanceThreshold) . sortBy (\(_, d1) (_, d2) -> compare d1 d2)) $ fmap (map (\(s, d) -> (s, d / fromIntegral (length s)))) $ fmap (zip e) (sequence (map (interDistance c) e))
--    where e = powerSet' es

interDistance :: DBC -> [Entity] -> IO Double
interDistance c el = if length el < 2 then return 1.0 else fmap (listMean) $ sequence $ map (\[e1, e2] -> distance' c wikilinks e1 e2) $ combinations 2 el

combinations 0 _ = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

lTuple2 [x, y] = (x, y)

powerSet [] = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)

powerSet' = filter (\s -> length s > 1) . powerSet

powerSet'' = filter (\s -> length s > 0) . powerSet

forOneConcat f l = foldr (:) [] $ map (\i -> let e = l !! i in [f e] ++ (filter (\x -> not (x == e)) l)) [0.. length l - 1]

uniqueGroupings :: (Eq a) => [a] -> [[[a]]]
uniqueGroupings [] = [[[]]]
uniqueGroupings (x:y:xs) = let rem = uniqueGroupings (y:xs) in (foldr (++) [] (map (forOneConcat ((:) x)) rem)) ++ (map ((++) [[x]]) rem)
uniqueGroupings (x:xs) = [[[x]]]

closestEntities c p e = fmap ((take 60) . (map fst) . sortBy (\(_, b) (_, d) -> compare b d) . filter (\(e, d) -> (d < distanceThreshold'))) $ (\es -> fmap (zip es) $ sequence $ map (\i -> ndistance c p [e] [i]) es) =<< fmap ((map fst) . filter (\(e, t) -> t)) =<< (\es -> return $ fmap (zip es) $ sequence $ map (isThing c) es) =<< setExtract c (prop e wikilinks)

bestMatch c tomatch poss n = fmap ( (take n) . (sortBy (\(_, b) (_, d) -> compare b d)) . (zip poss) ) $ sequence $ map (distance c wikilinks tomatch) poss

vf x = x == x && x /= 1/0

ndistance c p el1 el2 = do
    related <- setExtract c =<< setUnion c ((map (\e -> prop e p) el1) ++ (map (\e -> prop e p) el2))
    dists1 <- sequence $ map (\e -> distance c p el1 [e]) related
    dists2 <- sequence $ map (\e -> distance c p el2 [e]) related
    let q90 l = listMean l + 3 * stddev (filter (\x -> x /= 1/0 && x == x) l) in
    --let q90 l = foldl max 0.0 l in    
	let deltas = map abs (zipWith (\x y -> if (x /= x || x == 1/0) then (let mv = q90 dists1 in (max y mv) / (min y mv))
	    else (if (y /= y || y == 1/0) then (let mv = q90 dists2 in (max x mv) / (min x mv))
	    else (max x y) / (min x y)) ) dists1 dists2) in
    	    return $ listMean deltas
    {-
    let deltas = map abs $ zipWith (-) dists1 dists2 in
	let valid = filter vf deltas in
	    return $ (fromIntegral (length deltas) / fromIntegral (length valid)) * (listMean valid) 
    -}
    --d1 <- distance c wikilinks el1 el2
    --d2 <- distance c category el1 el2

distance c p el1 el2 = do
    rs1 <- sequence $ map (redirect c) el1
    rs2 <- sequence $ map (redirect c) el2
    fmap listMean $ sequence $ map (\(e1, e2) -> distance' c p e1 e2) $ combs rs1 rs2

synonymCheck c e1 e2 = fmap (== 1.0) $ distance c wikilinks [e1] [e2]



-- Atomic

redirect c e = fmap (fromMaybe e . fromEither Nothing) $ R.runRedis c $ R.get (B.append e "#red")

distance' :: DBC -> Property -> Entity -> Entity -> IO Double
distance' c p e1 e2 = do
    s1 <- setSizec c wl1
    s2 <- setSize c wl2
    d1 <- setSize c ds1
    d2 <- setSize c ds2
    si <- setIntersection c [wl1, wl2] >>= setSize c
    if d1 == 0 then 
        if d2 == 0 then return ((s1 + s2) / (2.0 * si)) else fmap (foldl1 min) (setExtract c ds2 >>= (sequence . (map (distance' c p e1))))
    else if d2 == 0 then fmap (foldl1 min) (setExtract c ds1 >>= (sequence . (map (distance' c p e2)))) 
    else do
        es1 <- setExtract c ds1
        es2 <- setExtract c ds2
        fmap (foldl1 min) $ sequence [(distance' c p e1 e2)| e1 <- es1, e2 <- es2]
    where (wl1, wl2, ds1, ds2) = (prop e1 p, prop e2 p, prop e1 disambiguations, prop e2 disambiguations)

-- Database Stuff

type Entity = B.ByteString
type Property = B.ByteString
type Set = B.ByteString
type DBC = R.Connection

-- Properties

wikilinks = "sem"
newslinks = "nwl"
redirects = "red"
disambiguations = "dis"
category = "cat"

isThing :: DBC -> Entity -> IO Bool
isThing c e = fmap ((=="1") . (fromMaybe "0") . fromEither Nothing) $ R.runRedis c $ R.get (B.append e "#typ")

prop :: Entity -> Property -> Set
prop e p = B.concat [e, "#", p]

setUnion :: DBC -> [Set] -> IO Set
setUnion c ss = let ns = B.append "tempU" (B.concat ss) in R.runRedis c $ R.sunionstore ns ss >> return ns

setIntersection :: DBC -> [Set] -> IO Set
setIntersection c ss = let ns = B.append "tempI" (B.concat ss) in R.runRedis c $ R.sinterstore ns ss >> return ns

setSize :: DBC -> Set -> IO Double
setSize c s = fmap (fromIntegral . (fromEither 0)) $ R.runRedis c $ R.scard s

setExtract :: DBC -> Set -> IO [Entity]
setExtract c s = fmap (fromEither []) $ R.runRedis c $ R.smembers s

fromEither :: b -> Either a b -> b
fromEither def e = case e of Right b -> b; Left a -> def

combs l1 l2 = [(t1, t2) | t1 <- l1, t2 <- l2]

listMean l = (\l -> (sum l) / (fromIntegral $ length l)) $ filter (\x -> x /= 1/0 && x == x) l
