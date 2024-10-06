--- STLC and principal type inference

-- Exercise 1a
{-
e1 :: a -> b -> b
e2 :: (a -> a) -> (a -> a)
e3 :: ((a -> a) -> b) -> b
e4 :: (a -> (a -> b)) -> a -> b 
e5 :: untypable because \y -> y y is untypable
e6 :: (a -> b) -> (c -> a) -> (c -> b)
-}


-- Exercise 1b
fn1 :: a -> b -> (a -> b -> c) -> c
fn1 = \x y z -> z x y
fn2 :: a -> b -> (a -> b -> c) -> (a,b,c)
fn2 = \x y z -> (x,y,z x y)
fn3 :: ([a] -> b) -> a -> b
fn3 = \f x -> f [x]
fn4 :: ((a -> a) -> b) -> b
fn4 = \x -> x (\x -> x)

-- Exercise 1c (optional)
{-
I do not think that we can take a function as an argument and return something of the same type for sure.
mysterylam = ??
-}

-- Exercise 1d (optional)
mysteryfn = undefined

--- Bidirectional typing

data Ty = TV Int | Fn Ty Ty
    deriving (Show,Eq)

data Expr = V Int | A Expr Expr | L Int Expr | Ann Expr Ty
    deriving (Show,Eq)

bcomp = L 0 $ L 1 $ L 2 $ A (V 0) (A (V 1) (V 2))

oneid = A (Ann (L 0 $ L 1 $ A (V 0) (V 1)) (Fn (Fn (TV 0) (TV 0)) (Fn (TV 0) (TV 0)))) (L 0 $ V 0)

type TyCxt = [(Int,Ty)]

check :: TyCxt -> Expr -> Ty -> Bool
synth :: TyCxt -> Expr -> Maybe Ty

-- Exercise 2 (optional)
check = undefined
synth = undefined

--- Programming in untyped lambda calculus

-- Encodings of booleans and natural numbers from class
{-
true = \x.\y.x
false = \x.\y.y
not = \b.\x.\y.b y x
and = \b.\c.b c false -- if 'b' is true, return 'c', otherwise 'false'
zero = \f.\x.x
one = \f.\x.f x
two = \f.\x.f (f x)
three = \f.\x.f (f (f x))
succ = \n.\f.\x.f (n f x)
add = \m.\n.m succ n -- apply the successor function m times with start value n
mult = \m.\n.m (add n) 0 -- m times the iterated addition of n on the number zero
isZero = \n.n (\b.false) true --the start value of the iteration is true, if we have iterations we return false
or = \a.\b.a true b -- if 'a' is true, return 'true', otherwise 'b'
-}

--for these exercises I got help from the following url: https://hbr.github.io/Lambda-Calculus/lambda2/lambda.html#arithmetic
--because I was confused at first with how to write arithmetics in lambda calculus.
--Now I understand better.


-- Exercise 3a
{- the start value for isEven is true, on each iteration we swap the boolean
isEven = \n. n not true
similarly:
isOdd = \n.n not false
-}

-- Exercise 3b
{-
exp = \n.\m.m (mult n) one --m times the iterated multiplication of n
-}

-- Encodings of pairing and projections
{-
pair = \x.\y.\f.f x y
fst = \p.p (\x.\y.x)
snd = \p.p (\x.\y.y)
-}

-- Exercise 3c
{-
swap = \p.pair (snd p) (fst p)
and without using second and first:
swapBis = \p.p (\x.\y.pair y x)
-}

-- Exercise 3d
{-
swapIf = \b.\p.b (swap p) p 
if b is true we return swap p, otherwise we return p
-}

-- Exercise 3e (optional)
{-
fib = <your definition here>
-}

-- Exercise 3f (optional)
{-
pred = <your definition here>
-}

-- Exercise 3g (optional)
{-
we check that n <= m and m <= n
eqNat = \n.\m. (isZero (m pred n)) and (isZero (n pred m))
-}

-- Curry's and Turing's fixed point operators
{-
Y = \x.(\y.x(y y))(\y.x (y y))
Theta = (\x.\y.y (x x y)) (\x.\y.y (x x y))
-}

-- Exercise 3h (optional)
{-
collatz = <your definition here>
-}

