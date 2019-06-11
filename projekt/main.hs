import System.IO  
import Control.Monad
import Data.Char
import Data.List
import Data.Function

main = do
    -- tymczasowe wczytywanie z plików
    stopHandle <- openFile "stop.words" ReadMode
    stopWords <- hGetContents stopHandle

    wordsHandle <- openFile "words.words" ReadMode
    wordsWords <- hGetContents wordsHandle

    -- na razie wypisuje 10 najpopularniejszych słów
    print $ (take 10).frequency.(filterStop.words $ stopWords).clear.words $ wordsWords

    hClose stopHandle
    hClose wordsHandle

clear :: [String] -> [String]
clear s = filter (\x -> all (\y -> y `elem` ['a'..'z']++['A'..'Z']) x) s

filterStop :: [String] -> [String] -> [String]
filterStop w s = filter (\x -> not $ x `elem` w) s

frequency :: [String] -> [(Int, String)]
frequency s = sortBy (flip compare `on` fst) $ map (\x -> (length x, head x)) $ group.sort $ s