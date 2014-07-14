import Paths_proverb (getDataFileName)
import System.Random (RandomGen, getStdGen, randomRs)

main :: IO ()
main = do
	input <- getDataFileName "proverbs.txt" >>= readFile
	gen <- getStdGen
	let (Just item) = roulette (lines input) gen
	putStrLn item

roulette :: System.Random.RandomGen g => [a] -> g -> Maybe a
roulette [] _ = Nothing
roulette items gen =
	let ((item, _):rest) = zip items (randomRs (0, 1) gen) in
	Just $ roulette' 2 item rest

roulette' :: Int -> t -> [(t, Float)] -> t
roulette' _ chosen [] = chosen
roulette' depth chosen ((item, roll):rest) =
	let chosen' = if roll < 1 / (fromIntegral depth) then item else chosen in
	roulette' (depth + 1) chosen' rest
