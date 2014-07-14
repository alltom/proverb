{-# LANGUAGE ScopedTypeVariables #-}

import Paths_proverb (getDataFileName)
import System.Random (RandomGen, getStdGen, randomRs)
import Data.Maybe (fromJust)
import Data.List (foldl')

main :: IO ()
main = do
	input <- getDataFileName "proverbs.txt" >>= readFile
	gen <- getStdGen
	putStrLn $ fromJust $ roulette (lines input) gen

roulette :: RandomGen g => [a] -> g -> Maybe a
roulette [] _ = Nothing
roulette (item:items) gen =
	Just $ snd $ foldl' roulette' (2, item) (zip items ((randomRs (0, 1) gen) :: [Float]))
	where roulette' (depth, chosen) (item, roll) =
		(depth + 1, if roll < 1 / (fromIntegral depth) then item else chosen)
