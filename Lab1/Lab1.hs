import Data.Char
import System.IO

-- functions from the lab notes:

-- Sorting

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
    where
     ys = [a | a <- xs, a <= x]
     zs = [b | b <- xs, b > x]

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

-- Side Effects

hello :: IO()
hello = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")



-- EXERCISES:

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split xs = (take n xs, drop n xs)
    where
     n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
    where
     (ys,zs) = split xs 

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = (x <= y) && isSorted xs
    where 
     y = head xs

sortText :: [String] -> String
sortText [] = ""
sortText xs = unlines (msort xs)

main :: IO()
main = interact(sortText . lines)