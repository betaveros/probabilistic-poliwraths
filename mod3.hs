import Data.List
import Data.Function
import Control.Applicative
import Control.Monad
import Control.Arrow

getGrid :: String -> IO [[Float]]
getGrid filename = map (map read . filter ((>0) . length) . filter (',' `notElem`) . groupBy ((==) `on` (== ','))) . lines <$> readFile filename

getMod3 :: Int -> [a] -> [a]
getMod3 res xs = map fst $ filter ((== res) . (`mod` 3) . snd) $ zip xs [0..]

zipCoords :: [[a]] -> [[((Int,Int),a)]]
zipCoords xss = zipWith (\r xs -> zipWith (\c x -> ((r,c),x)) [0..] xs) [0..] xss

mod3Sum :: Int -> Int -> [[Float]] -> Float
mod3Sum rm cm = sum . concat . getMod3 rm . map (getMod3 cm)

mod3Slant s (r,c) = ((r-c+s+1) `mod` 3, (r+c+2) `mod` 3)
-- second coord: (r+c+2) or (s-r+2) or (2-c-s)

mod3SlantSum :: Int -> Int -> Int -> [[Float]] -> Float
mod3SlantSum s rm cm = sum . map snd . filter ((== (rm,cm)) . fst) . map (mod3Slant s *** id) . concat . zipCoords

dumpGrid :: Int -> IO ()
dumpGrid n = do
	g <- getGrid ("s" ++ show n ++ ".txt")
	putStrLn $ "== " ++ show n ++ " =="
	forM_ [0..2] (\rm ->
		putStrLn $ intercalate "\t" $ map (\cm -> show $ mod3SlantSum n rm cm g) [0..2])

main = mapM_ dumpGrid [3..25]
