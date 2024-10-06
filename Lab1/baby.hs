doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = (if x > 100 then x else x*2) + 1  

lelist :: [Int] -> [Int] -> Bool
lelist [] ys = True
lelist (x:xs) [] = False
lelist (x:xs) (y:ys) = x < y || (x == y && lelist xs ys )

split xs = (take n xs ,drop n xs)
    where
     n = length xs `div` 2

msort [] = []
msort [x] = [x]
msort xs = merge (msort xs1) (msort xs2)
    where 
     (xs1,xs2) = split xs

eosplit [] = ([],[])
eosplit (x:xs) = (x:zs,ys)
    where
     (ys,zs) = eosplit xs 

