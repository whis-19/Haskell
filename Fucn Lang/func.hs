double :: Num a => a -> a
double x = x + x

headz (x:_)=x
tailz (_:x)=x

double1 :: Num a => a -> a
double1 x = x + x

doubleThedouble x = double (double x)
yes x = take (doubleThedouble x) [1..10000000]
--Num(+,-,*,/,'div')
--Eq(==,/=)
--Ord(<,<=,>,>=)


--[1,2,3]
--1:[2:[3:[]]]
--list 'F' ->"FUCK"

-- list :: [a] -> [a]
-- list [x] = [x]++[x] 

list :: a -> [a]
list x = [x]

-- prd :: Num a => [a]-> a
-- prd = product [1..6]

fac x = product [1..x]

summ xs= sum xs

avgg xs = (sum xs) `div` (length xs)
--head


first :: [a] -> a
first [] = error "FUCK OFF, WRONG INPUT YOU ASIAN"
first xs = head xs 

taill :: [a] -> [a]
taill [] = []
taill xs = tail xs 

lastt :: [a] -> a
lastt [] = error "niggaaaa"
lastt xs = head (reverse xs)

--lastt xs = head (reverse xs)

concc :: [a] -> [a] -> [a]
concc xs ys = xs ++ ys

firstconcc :: a -> [a] -> [a]
firstconcc x xs = x : xs

lastconcc :: a -> [a] -> [a]
lastconcc x xs = xs ++ list x

-- BHUT MSLA HAI
-- listTOlist :: [a] -> [b] -> [c] -> ([y])
-- listTOlist xs ys zs = (xs,ys,zs) 

ListSetToList :: [[a]] -> [a]
ListSetToList xss = 

moddd a b = a `mod` b







