import System.IO  
import Control.Monad
import System.Environment ( getArgs )
import Data.Char
import Data.List
import Data.Function
import Data.Typeable

-- uruchamiane jako "main.exe [liczba słów do wypisania] [plik ze stopami] [plik do analizy]"
-- potem będzie tam można wrzucić ich kilka
main = do
    args <- getArgs
    let number = read $ head $ args :: Int
    -- print $ typeOf number
    -- print number
    stops <- readFile (head.tail $ args)
    let contents = map readFile (tail.tail $ args)
    -- tak, nie znam się na typach w haskellu
    -- print $ typeOf contents
    -- print $ words stops
    ((take number).frequency.(filterStop.words $ stops).clear.words <$> head contents) >>= print

clear :: [String] -> [String]
clear s = filter (\x -> all (\y -> y `elem` ['a'..'z']++['A'..'Z']) x) s

filterStop :: [String] -> [String] -> [String]
filterStop w s = filter (\x -> not $ x `elem` w) s

frequency :: [String] -> [(Int, String)]
frequency s = sortBy (flip compare `on` fst) $ map (\x -> (length x, head x)) $ group.sort $ s

