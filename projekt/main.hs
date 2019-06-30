import System.IO  
import Control.Monad
import System.Environment ( getArgs )
import Data.Char
import Data.List
import Data.Function
import Control.Arrow
import Data.Typeable

-- uruchamiane jako "main.exe [liczba słów do wypisania] [plik ze stopami] [plik do analizy 1] [plik do analizy 2] ..."
-- przykładowo ./main.exe 10 ./data/stop.words ./data/lotr.txt ./data/KJB.txt
main = do
    args <- getArgs
    let number = read $ head args :: Int
    stops <- readFile (head.tail $ args)
    let contents = map readFile (tail.tail $ args)
    let stats = map (\ x -> take number . frequency . (filterStop . words $ stops) . clear . words <$> x) contents
    mapM (>>= print) [jaccard <$> s1 <*> s2 | s1 <- stats, s2 <- stats]
    -- mapM (>>= print) stats

clear :: [String] -> [String]
clear s = map (map toLower) $ filter (all (\ y -> y `elem` ['a' .. 'z'])) s

filterStop :: [String] -> [String] -> [String]
filterStop w = filter (`notElem` w)

frequency :: [String] -> [(Int, String)]
frequency s = sortBy (flip compare `on` fst) $ map (length Control.Arrow.&&& head) $ group.sort $ s

jaccard :: [(Int, String)] -> [(Int, String)] -> Double
jaccard stats1 stats2 = fromIntegral (statSum(map(\ x -> intersection (find (\ y -> snd y == snd x) stats2) x) stats1))/fromIntegral (statSum(map (\ x -> statUnion (find (\ y -> snd y == snd x) stats2) x) stats1))

statSum :: [(Int, String)] -> Int
statSum = foldl (\ a (f, _) -> a + f) 0

intersection :: Maybe (Int, String) -> (Int, String) -> (Int, String)
intersection Nothing (num1, token1) = (0, token1)
intersection (Just (num1, token1)) (num2, token2) = if num1 > num2 then (num2, token2) else (num1, token1)

statUnion :: Maybe (Int, String) -> (Int, String) -> (Int, String)
statUnion Nothing x = x
statUnion (Just (num1, token1)) (num2, token2) = if num1 > num2 then (num1, token1) else (num2, token2)