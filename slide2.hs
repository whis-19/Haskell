qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (n:ns) = qsort xs ++ [n] ++ qsort ys
    where
        xs = [x | x <- ns, x >= n]
        ys = [y | y <- ns, y < n]

add1 x y = x+y 
add = \ x -> (\y-> x + y)

add2 x y = [x+y |x>y]


double n = n + n
add2xinput n = map (\x -> x + double n) [1,2,3]


fuck n = map (\x -> x*2+1+n)[1,2,3]

getEmpty = [(x,y)|y<-[4,5],x<-[1,2,3],y<x]


getList = [(x,y)|x<-[1,2,3],y<-[4,5]]
getList1 = [(x,y)|y<-[4,5],x<-[1,2,3]]


factor x = [n|n<-[1..(x-1)],x `mod` n == 0]

perNum x = if sum (factor x) == x then True else False

nthreturn n xs = last (take n xs)

sumList :: Num a => [a] -> a
sumList [] = 0 
sumList (x:xs) = x + sumList xs  

maxF :: (Ord a,Num a)  => [a] -> a
maxF [] = 0
maxF (x:xs) = if ( x > maxF xs) then x else maxF xs

max2 :: (Ord a,Num a)  => [a] -> a
max2 [] = 0
max2 xs = qsort xs !! 1 



-------------------------------------------

toDigit :: Int -> [Int]
toDigit 0 = []
toDigit n = toDigit (n `div` 10) ++ [n `mod` 10]

isArmstrong :: Int -> Bool
isArmstrong n = n == sum (map (^ length (toDigit n)) (toDigit n))
listOfArmstrong m = [x | x <- [0..m] , isArmstrong x]






isPalindrome :: Eq a => [a] -> Bool
isPalindrome m = m == reverse m

calculator :: Num a => a -> a -> Char -> a 
calculator x y op = if op == '+' then x + y
                     else if op == '-' then x - y
                     else if op == '*' then x * y
                     --else if op == '/' then x `div` y
                     else error "Wrong Operator"




