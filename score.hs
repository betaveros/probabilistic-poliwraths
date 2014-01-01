#!/usr/bin/env runhaskell

import Data.List
import Data.Char
import Control.Applicative
import Control.Monad
import Data.Function
import System.Environment

getGrid :: String -> IO [[Float]]
getGrid filename = map (map read . filter ((>0) . length) . filter (',' `notElem`) . groupBy ((==) `on` (== ','))) . lines <$> readFile filename

bad :: Float -> Float -> Float
bad a b = (a - b)^2

totalBadness :: [[Float]] -> [[Float]] -> Float
totalBadness grid0 grid1 = sum $ concat $ zipWith (zipWith bad) grid0 grid1

score :: Float -> Float
score b = (max 0 $ 20 - sqrt b) ^ 2 / 4

main = do
	args <- getArgs
	grid0 <- getGrid $ args !! 0
	grid1 <- getGrid $ args !! 1
	let b = totalBadness grid0 grid1 in
		putStrLn $ "badness " ++ show b ++ "\tscore " ++ show (score b)
