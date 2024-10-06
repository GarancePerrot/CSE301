import Data.List

--- Zipping exercises

-- Exercise 1a
my_zip :: [a] -> [b] -> [(a,b)]
my_zip xs ys = zipWith (\a b -> (a,b)) xs ys

-- Exercise 1b
my_zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
my_zipWith f xs ys = map (\(a,b) -> (f a b)) (zip xs ys)

-- Exercise 1c (optional)
my_transpose :: [[a]] -> [[a]]
my_transpose = undefined

--- Folding exercises

-- Exercise 2a

altsum :: Num a => [a] -> a
altsum = foldr (-) 0

-- Exercise 2b

my_intersperse :: a -> [a] -> [a]
my_intersperse i xs = foldr (\x y -> if null y then [x] else x:i:y) [] xs
-- the if statement in the lambda function is to avoid the separator at the end of the list

-- Exercise 2c
my_tails :: [a] -> [[a]]
my_tails xs = foldr (\_ bs -> bs ++ (tail bs)) [] xs

-- Exercise 2d (optional)
--for this question I asked Chatgpt which gave me the idea to use zip and compare the pairs one by one 
binaryPrefix :: Eq a => (a , a) -> Bool -> Bool
binaryPrefix _ False = False
binaryPrefix (y,x) b = (y==x) && b

my_isPrefixOf :: Eq a => [a] -> [a] -> Bool
my_isPrefixOf ys xs = foldr binaryPrefix True (zip ys xs)
--v is of type bool because it is the return type of binaryPrefix

-- Exercise 2e (optional)
my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile = undefined

-- Exercise 2f (optional)
-- (your proof here)

--- Difference lists

type DiffList a = [a] -> [a]

toDL :: [a] -> DiffList a
toDL xs = (xs++)

fromDL :: DiffList a -> [a]
fromDL dxs = dxs []

cons :: a -> DiffList a -> DiffList a
cons x dxs = (x:) . dxs

snoc :: DiffList a -> a -> DiffList a
snoc dxs x = dxs . (x:)

-- Exercise 3a
toDLrev :: [a] -> DiffList a
toDLrev xs = foldr (\x acc -> snoc acc x) id xs

-- Exercise 3b
my_reverse :: [a] -> [a]
my_reverse xs = fromDL (toDLrev xs)

naive_reverse :: [a] -> [a]
naive_reverse []     = []
naive_reverse (x:xs) = naive_reverse xs ++ [x]

-- Exercise 3c
{-
We indeed have an isomorphism between fromDL and toDL. 
- Let us prove that for any xs :: [a] , fromDL (toDL xs) = xs
fromDL (toDL xs) = fromDL (xs ++) = (xs ++) [] = xs ++ [] = xs
- Let us prove that for any dxs :: DiffList a , toDL (fromDL dxs) = dxs
toDL (fromDL dxs) = toDL (dxs []) = (dxs []) ++ = dxs
For the last equality (dxs []) ++ = dxs , when we apply dxs to [] we "close the door" to form
a complete list, but when we put the ++ operator we "open the door" again to any new list to which 
dxs can be attached to. 
Therefore the two functions are isomorphic.
-}

--- Regular expression matching

data RegExp = Zero | One
            | C Char
            | Plus RegExp RegExp | Times RegExp RegExp
            | Star RegExp
  deriving (Show,Eq)

accept :: RegExp -> String -> Bool

accept e w = acc e w null
--determines whether the string w is in the language defined by the regular expression e

-- Exercise 4a
acc :: RegExp -> String -> (String -> Bool) -> Bool
acc Zero          w k = False
acc One           w k = k w
acc (C a) w k = case w of
                   (x:xs) | x == a -> k xs
                   _              -> False
--we check the first element and then call the continuation function
acc (Plus e1 e2)  w k = acc e1 w k || acc e2 w k
acc (Times e1 e2) w k = acc e1 w (\c -> acc e2 c k) 
--here we change the continuation function to be the checking of the remaining portion of the string after matching e1

-- Exercise 4b (optional)
acc (Star e)      w k = undefined

