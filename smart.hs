#!/usr/bin/env runhaskell
-- refactoring!
import Data.Array
import Data.List
import Data.Char
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Function
import System.Environment
import Text.Printf

-- logic stuff

iverson :: (Num a) => Bool -> a
iverson True  = 1
iverson False = 0

{-
mod3score :: (Num a) => Int -> Int -> Int -> a
mod3score sess r c = g $ (c - r + 1 - sess) `mod` 3
	where
		g 0 = 8 -- good
		g 1 = 4 -- meh
		g 2 = 0 -- bad
-}
mod3score :: (Num a) => Int -> Int -> Int -> a
mod3score sess r c = g ((r - c + sess + 1) `mod` 3) ((r + c + 2) `mod` 3)
	where
		g 2 2 = 8
		g 2 1 = 8
		g 2 0 = 8
		g 1 2 = 6
		g 1 1 = 4
		g 1 0 = 2
		g 0 _ = 0

viiScore :: (Num a) => Int -> Int -> Int -> a
viiScore sess r c = g ((r - c + sess + 1) `mod` 3) ((r + c + 2) `mod` 3)
	where
		g 2 2 = 8
		g 2 1 = 8
		g 2 0 = 7
		g 1 2 = 6
		g 1 1 = 4
		g 1 0 = 2
		g 0 _ = 0

type Grid = Array (Int,Int)

close :: (Ord a, Num a) => a -> a -> Bool
close a b = -1 <= (b - a) && (b - a) <= 1

safeRange x1 x2 = [max x1 0 .. min x2 29]

kingList :: (Int,Int) -> [(Int,Int)]
kingList (r,c) = [(r1, c1) | r1 <- safeRange (r-1) (r+1), c1 <- safeRange (c-1) (c+1)] \\ [(r,c)]

gridToLocs :: Grid Int -> [(Int,Int)]
gridToLocs = map fst . filter ((== 1) . snd) . assocs

grid :: (Int -> Int -> a) -> Grid a
grid f = array ((0,0),(29,29)) [((r,c), f r c) | r <- [0..29], c <- [0..29]]

locsToGrid :: (Num a) => [(Int,Int)] -> Grid a
locsToGrid locs = grid (\r c -> iverson $ (r,c) `elem` locs)

kingsGrid :: Grid Int -> Grid Int
kingsGrid g = grid (\r c -> sum $ map (g !) $ kingList (r,c))

moveList :: ((Int, Int) -> Int) -> (Int, Int) -> [(Int, Int)]
moveList scorer loc = reverse . sortBy (compare `on` scorer) $ kingList loc

km 0 = -10
km 1 = 0
km _ = 10

m3kMetric rw cw sess (r,c) kings = 10000*((mod3score sess r c) + km kings) + rw*r + cw*c
viiMetric rw cw sess (r,c) kings = 10000*((viiScore sess r c) + km kings) + rw*r + cw*c

coordGrid = [[(r,c) | c <- [0..29]] | r <- [0..29]]

-- metric :: (row, col) -> sess -> score
getMetricGrid :: ((Int, Int) -> Int -> Int) -> Grid Int -> Grid Int
getMetricGrid metric lastGrid = let kg = kingsGrid lastGrid in grid (
	\r c -> metric (r, c) (kg ! (r,c)))

getM3kScorer rw cw s lastGrid = (!) $ getMetricGrid (m3kMetric rw cw s) lastGrid

getViiScorer rw cw s lastGrid = (!) $ getMetricGrid (viiMetric rw cw s) lastGrid

getNaiveScorer rw cw _ (r, c) = r*rw + c*cw

testScorer _ _ (r, c) = r

moveGrid :: ((Int, Int) -> Int) -> [(Int,Int)] -> [(Int,Int)]
moveGrid s ls = f s ls []
	where
		f _ [] _ = []
		f scorer (loc:locs) usedLocs =
			let ps = moveList scorer loc;
				mloc = head $ (ps \\ locs) \\ usedLocs in
				mloc : f scorer locs (mloc : usedLocs)

m3kGuess s (r, c) kings = iverson $ mod3score s r c + 2*kings >= 12

-- IO!

readGrid :: String -> IO (Grid Int)
readGrid filename = do
	cont <- readFile filename
	return $ grid (\r c -> (map (map read . filter ((>0) . length) . filter (',' `notElem`) . groupBy ((==) `on` (== ','))) $ lines cont) !! r !! c)

-- cache the grids we've read from the filesystem in a State monad
type SIO a = StateT (Map.Map Int (Grid Int)) IO a

getSession :: Int -> SIO (Grid Int)
getSession s = do
	maybeGrid <- Map.lookup s <$> get
	case maybeGrid of
		Just g  -> return g
		Nothing -> do
			grid <- lift $ readGrid ("s" ++ show s ++ ".txt")
			modify $ Map.insert s grid
			return grid

-- strategies at last

type Strategy = Int -> SIO (Grid Float)

dumb :: Strategy
dumb s = do
	lastGrid <- getSession $ s - 1
	return $ let kg = kingsGrid lastGrid in grid (\r c -> m3kGuess s (r, c) (kg ! (r, c)))

-- (lastGrid -> (r, c) -> score)
metricBase :: (Grid Int -> (Int, Int) -> Int) -> (Grid Int -> (Int, Int) -> Int) -> Strategy
metricBase m1 m2 s = do
	lastGrid <- getSession $ s - 1
	lastLastGrid <- getSession $ s - 2
	return $ locsToGrid $ moveGrid (m1 lastGrid) $ reverse $ sortBy (compare `on` m2 lastLastGrid) $ gridToLocs lastGrid

brainBase :: Int -> Int -> Int -> Int -> Strategy
brainBase rw cw rwo cwo s = metricBase (getM3kScorer rw cw s) (getM3kScorer rwo cwo (s-1)) s

viiBase :: Int -> Int -> Int -> Int -> Strategy
viiBase rw cw rwo cwo s = metricBase (getViiScorer rw cw s) (getViiScorer rwo cwo (s-1)) s

naiveM3kBase :: Int -> Int -> Int -> Int -> Strategy
naiveM3kBase rw cw rwo cwo s = metricBase (getM3kScorer rw cw s) (getNaiveScorer rwo cwo) s

naiveViiBase :: Int -> Int -> Int -> Int -> Strategy
naiveViiBase rw cw rwo cwo s = metricBase (getViiScorer rw cw s) (getNaiveScorer rwo cwo) s

smartBase :: Int -> Int -> Strategy
smartBase rw cw s = metricBase (getM3kScorer rw cw s) (getM3kScorer rw cw (s-1)) s

smartGen :: Int -> Strategy
smartGen lexf = smartBase (100*lexf) lexf

scalarMultiply :: (Num a) => a -> Grid a -> Grid a
scalarMultiply c xss = (c*) <$> xss

weightedAverageGridList :: (Num a, Fractional a) => [(a,Grid a)] -> Grid a
weightedAverageGridList gs = grid $ \r c -> let (wts, wtVals) = unzip [(wt, wt * (g ! (r, c))) | (wt, g) <- gs] in sum wtVals / sum wts

averageGridList :: (Num a, Fractional a) => [Grid a] -> Grid a
averageGridList = weightedAverageGridList . zip (repeat 1)

weightedAverageStrategyList :: [(Float,Strategy)] -> Strategy
weightedAverageStrategyList ss n = weightedAverageGridList <$> sequence [(\sr -> (wt, sr)) <$> s n | (wt, s) <- ss]

averageStrategyList :: [Strategy] -> Strategy
averageStrategyList ss n = averageGridList <$> sequence [s n | s <- ss]

sharpen :: Float -> Float
sharpen f = if f >= 0.5 then 1 else 0

sharpenStrategy :: Strategy -> Strategy
sharpenStrategy = fmap $ fmap $ fmap sharpen

fourDirs = [(0,1), (0,-1), (1,0), (-1,0)]
eightDirs = [( 100,1),
	( 100,-1),
	(-100,1),
	(-100,-1),
	(1,100),
	(1,-100),
	(-1,100),
	(-1,-100)]
octillery :: Strategy
octillery = averageStrategyList [smartBase rw cw | (rw, cw) <- eightDirs]

staryu :: Strategy
staryu = averageStrategyList $ dumb : map (uncurry smartBase) fourDirs

starmie :: Strategy
starmie = averageStrategyList $ dumb : dumb : map (uncurry smartBase) fourDirs

gastly :: Strategy
gastly = averageStrategyList $ [brainBase rw cw rwo cwo | (rw, cw) <- fourDirs, (rwo, cwo) <- fourDirs]

haunter :: Strategy
haunter = weightedAverageStrategyList $ (4,dumb) : [(1,brainBase rw cw rwo cwo) | (rw, cw) <- fourDirs, (rwo, cwo) <- fourDirs]

deoxys :: Strategy
deoxys = weightedAverageStrategyList $ (12,dumb) : [(1, brainBase rw cw rwo cwo) | (rw, cw) <- fourDirs, (rwo, cwo) <- fourDirs] ++ [(1, viiBase rw cw rwo cwo) | (rw, cw) <- fourDirs, (rwo, cwo) <- fourDirs]

regigigas :: Strategy
regigigas = weightedAverageStrategyList $ (18,dumb) : map ((,) 4) [brainBase rw cw rwo cwo | (rw, cw) <- fourDirs, (rwo, cwo) <- fourDirs] ++ map ((,) 12) [viiBase rw cw rwo cwo | (rw, cw) <- fourDirs, (rwo, cwo) <- fourDirs] ++ map ((,) 1) [naiveM3kBase rw cw rwo cwo | (rw, cw) <- fourDirs, (rwo, cwo) <- eightDirs] ++ map ((,) 8) [naiveViiBase rw cw rwo cwo | (rw, cw) <- fourDirs, (rwo, cwo) <- eightDirs]

ninetales :: Strategy
ninetales = averageStrategyList $ dumb : [smartBase rw cw | (rw, cw) <- eightDirs]

showRow :: (Show a) => [a] -> String
showRow = intercalate "," . map show

showFt :: Float -> String
showFt 0 = "0"
showFt 1 = "1"
showFt f = printf "%f" f

showFtRow :: [Float] -> String
showFtRow = intercalate "," . map showFt

showFtGrid :: Grid Float -> String
showFtGrid = unlines . map (showFtRow . map snd) . groupBy ((==) `on` (fst . fst)) . assocs

dist a b = abs (a - b)
excess a b = if a >= b then (a-b)^2 else 0

gridExcess :: Grid Float -> Grid Float -> Float
gridExcess g1 g2 = sum $ zipWith excess (elems g1) (elems g2)
badnessPair g1 g2 = (gridExcess g1 g2, gridExcess g2 g1)

selectAlgorithm = f where
	f "dumb" = dumb
	f "smart" = smartGen 0
	f "smart1" = smartGen 1
	f "smartn" = smartGen (-1)
	f "medium" = averageStrategyList [dumb, smartGen 0]
	f "doduo"  = averageStrategyList [smartGen 1, smartGen (-1)]
	f "dodrio" = averageStrategyList [dumb, smartGen 1, smartGen (-1)]
	f "octillery" = octillery
	f "staryu" = staryu
	f "starmie" = starmie
	f "ninetales" = ninetales
	f "gastly" = gastly
	f "haunter" = haunter
	f "deoxys" = deoxys
	f "regigigas" = regigigas
	f s = error ("No such strategy: " ++ s ++ " (dumb, smart, etc.)")

score :: Float -> Float
score b = (max 0 $ 20 - sqrt b) ^ 2 / 4

scorePair :: (Float, Float) -> Float
scorePair = score . uncurry (+)

dumpPair :: (Float, Float) -> IO ()
dumpPair (a, b) = printf "%9.6f sc=%9.6f (%f,%f)\n" (a+b) (score $ a+b) a b

sumPairs :: (Num a) => [(a,a)] -> a
sumPairs = sum . map (uncurry (+))

sumScores :: [(Float,Float)] -> Float
sumScores = sum . map scorePair

main = void $ flip runStateT Map.empty $ do
	args <- lift getArgs
	if length args == 2 then do
		guessGrid <- selectAlgorithm (head args) $ read $ args !! 1
		lift $ putStrLn $ showFtGrid guessGrid
		return ()
	else if length args `elem` [1,3] then do
		range <- return $ if length args == 3
			then [read (args !! 1)..read (args !! 2)]
			else [3..32]
		guessedGrids <- mapM (selectAlgorithm $ head args) range
		realGrids <- mapM (fmap (fmap fromIntegral) . getSession) range
		badnesses <- return $ map (uncurry badnessPair) $ zip guessedGrids realGrids
		lift $ do
			mapM dumpPair badnesses
			putStrLn $ "A>B: " ++ show (sum $ map fst badnesses)
			putStrLn $ "A<B: " ++ show (sum $ map snd badnesses)
			putStrLn $ "avg badness\t" ++ show ((sumPairs badnesses) / (fromIntegral $ length range))
			putStrLn $ "avg score\t" ++ show ((sumScores badnesses) / (fromIntegral $ length range))
	else error "1 to 3 arguments"
