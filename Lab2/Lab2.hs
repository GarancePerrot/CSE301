--- Theory exercises (from Lecture 1 notes)

-- Exercise 2.2
{-
f:: Either String (Both String String) -> Both (Either String String) (Either String String)
f (Left "Ride") = Pair (Right "Swim") (Right "Run")
f (Right (Pair "Swim" "Run")) = Pair (Left "Ride") (Right "Run")

g:: Both (Either String String) (Either String String) -> Either String (Both String String)
g (Pair (Right "Swim") (Left "Ride")) = Right (Pair "Swim" "Run")
g (Pair (Right "Swim") (Right "Run")) = Right (Pair "Swim" "Run")
g (Pair (Left "Ride") (Left "Ride")) = Left "Ride"
g (Pair (Left "Ride") (Right "Run")) = Left "Ride"

We want to know if Either a (Both b c) is isomorphic to Both (Either a b) (Either a c), 
in other words if g (f x) = x for all x in Either a (Both b c) and f (g y) = y for all y 
in Both (Either String String) (Either String String). 

However, g ( f (Left "Ride ) ) = g (Pair (Right "Swim") (Right "Run")) = Right (Pair "Swim" "Run")
which is not equal to Left "Ride". The property is not satisfied so they are not isomorphic.

-}

-- Exercise 3.1
{-
Let us prove that lists form a monoid under concatenation with nil as the identity element.
Let xs, ys, zs be lists. Let us recall the definition of concatenation: 
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

(1) [] ++ xs = xs is proved directly by definition of concatenation

(2) xs ++ [] = xs
Proof by structural induction:
- If xs is []
[] ++ [] = [] by (1)
- Assume P(xs) holds, let us prove P(x:xs).
(x:xs) ++ [] =  x : (xs ++ []) = x : xs  
The first equality is by definition of concatenation, the second is using the assumption.
Thus P(xs) holds for all lists xs.

(3) xs ++ (ys ++ zs) = (xs ++ ys) ++ zs   
Proof by structural induction on xs, treating ys and zs as constants.
- If xs is []
[] ++ (ys ++ zs) = ys ++ zs = ([] ++ ys) ++ zs
The first equality is by (1) on (ys ++ zs) and the second equality is by (1) on ys
- Assume P(xs) holds, let us prove P(x:xs).
(x:xs) ++ (ys ++ zs) = x : (xs ++ (ys ++ zs)) = x : ((xs ++ ys) ++ zs) 
= x:(xs ++ ys) ++ zs = ((x:xs) ++ ys) ++ zs
The 1st, 3rd, 4th equalities are by def of concatenation, the 2nd is by assumption.
Thus P(xs) holds for all xs.

We have proved that nil is the left and right identity for lists and the property of
associativity so lists form a monoid. 
-}

-- Exercises 3.2-3.4 (optional)


--- Programming exercises

-- Exercise 0a
doubleList :: [a] -> [a]
doubleList [] = []
doubleList (x:xs) = x:(x:doubleList xs)

-- Helper function for 0b:
isDoubled :: Eq a => a -> [a] -> Bool
isDoubled _ [] = False
isDoubled y (x:xs)
    | y == x = True
    | otherwise = False

-- Exercise 0b
firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled [] = Nothing
firstDoubled (x:xs)
    | isDoubled x xs = Just x
    | otherwise = firstDoubled xs

data Allergen = Nuts | Gluten | Soy | Dairy      deriving (Show, Eq)

type Recipe   = [Allergen]

type Name     = String
type Price    = Int
data Cupcake  = CC Name Recipe Price             deriving (Show,Eq)

r1, r2, r3, r4, r5 :: Recipe
r1 = [Gluten]
r2 = []
r3 = [Nuts]
r4 = [Dairy,Gluten]
r5 = [Soy]

onsale :: [Cupcake]
onsale = [CC "Chocolate Surprise" r1 200,
          CC "Lemon Mayhem" r2 150,
          CC "Peanut Butter Bliss" r3 150,
          CC "Yogurt Truly" r4 250,
          CC "Caramel Karma" r5 200]

-- Exercise 1a
priceRange :: Price -> Price -> [Cupcake] -> [Name]
priceRange _ _ [] = []
priceRange minPrice maxPrice (CC name _ price : cs)
    | price >= minPrice && price <= maxPrice = name : priceRange minPrice maxPrice cs
    | otherwise = priceRange minPrice maxPrice cs

--Helper function for 1b
isInList :: Eq a => a -> [a] -> Bool
isInList _ [] = False
isInList y (x:xs)
    | y == x = True
    | otherwise = isInList y xs

-- Exercise 1b
allergyFree :: [Allergen] -> [Cupcake] -> [Name]
allergyFree _ [] = [] 
allergyFree as (CC name recipe _ :cs) 
    | any  (\allergen -> isInList allergen recipe) as = allergyFree as cs
    | otherwise = name : allergyFree as cs
-- checks if any allergen in as is in the cupcake's recipe using isInList


type Tin = [Recipe]
data Spec = And Spec Spec | Or Spec Spec | Not Spec | HasCup Int Allergen  deriving (Show,Eq)

sampletin :: Tin
sampletin = [r3,r4,r2,r5]

-- Exercise 2a
checkSpec :: Spec -> Tin -> Bool
checkSpec _ [] = False
checkSpec (And s1 s2) tin = if (checkSpec s1 tin) && (checkSpec s2 tin) then True else False
checkSpec (Or s1 s2) tin = if (checkSpec s1 tin) || (checkSpec s2 tin) then True else False
checkSpec (Not s) tin = if (checkSpec s tin) then False else True
checkSpec (HasCup k x) tin = if isInList x (tin !! k) then True else False

-- Exercise 2b (optional)
checkSpec' :: Spec -> Tin -> Maybe Bool
checkSpec' (HasCup k x) tin = if (0<=k) && (k < length tin) && isInList x (tin !! k) then Just True else Nothing
checkSpec' spec tin = Just (checkSpec spec tin)

data Tree a b = Leaf a | Node b [Tree a b]  deriving (Show,Eq)

texample :: Tree Char Integer
texample = Node 1 [Node 2 [Leaf 'a', Leaf 'b'], Node 3 [Leaf 'c', Leaf 'd', Leaf 'e'], Node 4 []]

bst :: Tree () Char
bst = Node 'c' [Node 'a' [Leaf (), Node 'b' [Leaf (), Leaf ()]], Node 'd' [Leaf (), Leaf ()]]

--Helper for 3a (aka forest_canopy)
concatCanopy :: [Tree a b] -> [a]
concatCanopy [] = []
concatCanopy (subtree:ss) = (canopy subtree) ++ (concatCanopy ss)

-- Exercise 3a
canopy :: Tree a b -> [a]
canopy (Leaf a) =  [a]
canopy (Node _ children) = concatCanopy children

--Helper for 3b (aka forest_preorder)
concatPreOrder :: [Tree a b] -> [Either a b]
concatPreOrder [] = []
concatPreOrder (subtree:ss) = (preorder subtree) ++ (concatPreOrder ss)

--Exercise 3b (optional) 
preorder :: Tree a b -> [Either a b]
preorder (Leaf a) = [Left a]
preorder (Node b children) = [Right b] ++ (concatPreOrder children)


-- Exercise 4

linearSortAcc :: Ord a => [a] -> [a] -> [a] -> [a] --inputs: input, current stack, current result
linearSortAcc [] [] [] = []
linearSortAcc [] [] res = res
linearSortAcc [] stack res = res ++ stack
linearSortAcc (x:xs) stack res
    | stack == [] = linearSortAcc xs [x] res
    | x <= t = linearSortAcc xs (x:stack) res --push x on stack
    | otherwise = linearSortAcc (x:xs) (tail stack) (res ++ [t]) --pop the top item on the stack to the end of the output
    where t = head stack

linearSort :: Ord a => [a] -> [a]
linearSort xs = linearSortAcc xs [] []


-I will do them later
-- Exercise 5a (optional)
counterexample :: [Int]
counterexample = undefined

data Bin = L | B Bin Bin  deriving (Show,Eq)

-- Exercise 5b (optional)
fromBin :: Bin -> [Int]
fromBin = undefined
toBin :: [Int] -> Maybe Bin
toBin = undefined

