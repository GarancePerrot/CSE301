import Control.Monad.State
import Control.Monad.Fail
import System.Random
import Data.List

data Expr = Con Double | Sub Expr Expr | Div Expr Expr
    deriving (Show,Eq)

e1 = Sub (Div (Con 2) (Con 4)) (Con 3)
e2 = Sub (Con 1) (Div (Con 2) (Con 2))
e3 = Div (Con 1) (Sub (Con 2) (Con 2))

-- Exercise 1a
evalSafe :: Expr -> Maybe Double
evalSafe (Con c)     = return c
evalSafe (Sub e1 e2) = do {x1 <- evalSafe e1
                           ; x2 <- evalSafe e2
                           ; return (x1-x2)}
evalSafe (Div e1 e2) = do {x1 <- evalSafe e1
                           ; x2 <- evalSafe e2
                           ; if x2 /= 0 then return (x1 / x2) else Nothing}                          

-- Exercise 1b
evalSafeMF :: MonadFail m => Expr -> m Double
evalSafeMF (Con c) = return c
evalSafeMF (Sub e1 e2) = do {x1 <- evalSafeMF e1
                           ; x2 <- evalSafeMF e2
                           ; return (x1-x2)}
evalSafeMF (Div e1 e2) = do {x1 <- evalSafeMF e1
                           ; x2 <- evalSafeMF e2
                           ; if x2 /= 0 then return (x1 / x2) else fail "Error : division by 0"}                          
     

{- different outputs of evalSafeMF:
e0 = (Con 7)
e1 = Div (Con 10) (Con 2)
e2 = Sub (Con 5) (Con 2)
e3 = Div (Con 3) (Con 0)
evalSafeMF e3 = *** Exception: user error (Error : division by 0)
 -}

evalWeird :: Expr -> StateT Int Maybe Double
evalWeird (Con c)    =
  get >>= \n ->
  put (n+1) >>= \_ ->
  return (if n `mod` 3 == 2 then 0 else c)
evalWeird (Sub e1 e2) =
  evalWeird e1 >>= \x1 ->
  evalWeird e2 >>= \x2 ->
  return (x1-x2)
evalWeird (Div e1 e2) =
  evalWeird e1 >>= \x1 ->
  evalWeird e2 >>= \x2 ->
  if x2 /= 0 then return (x1/x2) else lift Nothing
evalWeirdTop e = runStateT (evalWeird e) 0 >>= \(x,s) -> return x

-- Exercise 1c

evalWeird' :: MonadFail m => Expr -> StateT Int m Double
evalWeird' (Con c)    =
  get >>= \n ->
  put (n+1) >>= \_ ->
  return (if n `mod` 3 == 2 then 0 else c)
evalWeird' (Sub e1 e2) =
  evalWeird' e2 >>= \x2 ->
  evalWeird' e1 >>= \x1 ->
  return (x1-x2)
evalWeird' (Div e1 e2) =
  evalWeird' e2 >>= \x2 ->
  evalWeird' e1 >>= \x1 ->
  if x2 /= 0 then return (x1/x2) else fail "Error: division by 0"
evalWeirdTop' :: MonadFail m => Expr -> m Double
evalWeirdTop' e = runStateT (evalWeird' e) 0 >>= \(x,s) -> return x

data Bin a = L a | B (Bin a) (Bin a)
  deriving (Show,Eq)

mapBin :: (a -> b) -> Bin a -> Bin b
mapBin f (L x)     = L (f x)
mapBin f (B tL tR) = B (mapBin f tL) (mapBin f tR)

instance Functor Bin where
  fmap = mapBin

-- Exercise 2a
{- We want to show that mapBin is a functor.

1. Let us prove that mapBin id t = t for all binary trees t :: Bin a. Let x::a.
mapBin id (L x) = L (id x) = L x
mapBin id (B tL tR) = B (mapBin id tL) (mapBin id tR) = B tL tR

2. Let us prove by induction that mapBin (f . g) t = mapBin f (mapBin g t).

base case:
mapBin (f.g) (L x) = L ((f.g) x) = L (f (g x)) = mapBin f (L (g x))= mapBin f (mapBin g (L x))

inductive case: 
By hypothesis, assume that the proposition holds for tL and tR, let us prove it holds for (B tL tR)
mapBin (f.g) (B tL tR) = B (mapBin (f.g) tL) (mapBin (f.g) tR) 
                       = B (mapBin f (mapBin g tL)) (mapBin f (mapBin g tR))  by hypothesis
                       = mapBin f (B (mapBin g tL) (mapBin g tR))
                       = mapBin f (mapBin g (B tL tR))
-}

-- Exercise 2b
instance Monad Bin where
  return a = L a
  (L a) >>= f = f a 
  (B tL tR) >>= f = B (tL >>= f) (tR >>= f)

instance Applicative Bin where
  pure = return
  fm <*> xm = fm >>= \f -> xm >>= return . f

-- Exercise 2c (optional)
{- Let us prove that Monad Bin satisfies the monad laws.

- identity laws :
return x >>= f = (L x) >>= f = f x
Now let us prove by induction that t >>= return = t. 
base case:
(L x) >>= return = return (L x) = L x
inductive case:
Assume it holds for tL and tR, let use prove it for (B tL tR)
(B tL tR) >>= return = B (tL >>= return) (tR >>= return) = B tL tR

-composition laws:
Let us prove by induction that (t >>= f) >>= g = t >>= (\x -> (f x >>= g)). 
base case :
((L x) >>= f) >>= g = (f x) >>= g = (L x) >>= (\x -> (f x >>= g))
inductive case:
Assume it holds for tL and tR, let use prove it holds for (B tL tR)
((B tL tR) >>= f) >>= g = (B (tL >>= f) (tR >>= f)) >>= g
                        = B ((tL >>= f) >>= g) ((tR >>= f) >>= g)
                        = B (tL >>= (\x -> (f x >>= g))) (tR >>=  (\x -> (f x >>= g))) by hypothesis
                        = (B tL tR) >>= (\x -> (f x >>= g))
-}

-- Exercise 2d (optional)
{- Your thoughts go here -}

class Monad m => SelectMonad m where
  select :: [a] -> m a

instance SelectMonad [] where
  select = id

instance SelectMonad IO where
  select xs
    | not (null xs) = do i <- getStdRandom (randomR (0, length xs-1))
                         return (xs !! i)
    | otherwise     = fail "cannot select from empty list"

newtype Dist a = Dist { dist :: [(a,Rational)] }  deriving (Show)

instance Monad Dist where
  return x = Dist [(x,1)]
  xm >>= f = Dist [(y,p*q) | (x,p) <- dist xm, (y,q) <- dist (f x)]
  
-- We add the following standard boilerplate to derive instances of the
-- Functor and Applicative type classes, from the Monad instance above:
instance Functor Dist where
  fmap f xm = xm >>= return . f

instance Applicative Dist where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

instance SelectMonad Dist where
  select xs
    | not (null xs) = let n = length xs in Dist [(x, 1 / fromIntegral n) | x <- xs]
    | otherwise     = error "cannot select from empty list"

code :: SelectMonad m => m Char
code = do
  i <- select [0..3]
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalize :: Eq a => Dist a -> Dist a
normalize xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub", defined in Data.List, removes duplicates

-- Exercise 3a
coin :: SelectMonad m => m Bool
coin =  do i <- select [0,1]
           return ([True, False] !! i)

-- Exercise 3b
subset :: SelectMonad m => [a] -> m [a]
subset [] = return []
subset (x:xs) = do b <- coin
                   rest <- subset xs
                   return (if b then x:rest else rest)              

-- Exercise 3c
simulate :: Monad m => Int -> m Bool -> m Int
simulate n b
  | n <= 0 = return 0
  | otherwise = simulateAcc n b 0

-- Accumulator function : simulate n times, count the instances of True
simulateAcc :: Monad m => Int -> m Bool -> Int -> m Int
simulateAcc n b c
  | n <= 0    = return c
  | otherwise = do {res <- b
                   ; let c2 = if res then c+1 else c
                   ;simulateAcc (n-1) b c2}

-- Exercise 3d (optional)
genTree :: SelectMonad m => [a] -> m (Bin a)
genTree = undefined

