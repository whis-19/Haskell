-- double n = n + n
-- summtoL n = map 

add = \ x -> (\ y -> x + y )

as x y z = z - (\x y -> x + y ) x y
double = \ x -> x + x 

f :: Num a => a -> a -> a -> a
f z x y = z - (\x -> (\y -> x + y)) x y



merenam b = if(even b) then b+b else b
bachi xs = map merenam xs
bachisexy xs = map (\n -> if(even n) then n+n else n) xs
pubg = (\x -> if x > 7 then "Pro" else if x < 7 then "Noob" else  error "Bache Teri" )

bachisexypp :: Num a => a -> [a] -> [a]
bachisexypp x xs = map (\n -> x + 1 * 2 + x) xs


join xs ys = [ (x , y) | x <- xs , y <- ys, x < y]

factors n = [x^2 | x <- [1..n] , n `mod ` x == 0]

match n = [(n,k) | n<-[1..99],k<-[3]]

qsort [] = []
qsort (n:ns) = qsort xs ++ [n] ++ qsort ys
    where
        xs = [x | x<-ns , x<=n]
        ys = [y | y<-ns , y>n]

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = zip xs ys

--pairr xs = zip xs (tail xs)

add2 x y = [x+y | x<y ]
add2xinput x xs = map (\x -> x*2 ) xs

getEmpty = [(x,y) | y<-[4,5] , x<-[1,2,3] , y<x]

left = [x | x<-[1,2,3]]
right = [y | y<-[4,5]]
both = left ++ right

both2 = [x | x<-[1..5]] ++ [y | y<-[6..10]]

factor n = [x | x<-[1..n] , mod n x == 0]



factorial n = product [x | x <- [1..n]]

fact xs = [ factorial x | x<-xs]

calculator :: Num a => a -> a -> Char -> a
calculator x y opt = if (opt=='+') then x+y
                     else if (opt=='-') then x-y 
                    --else if (opt=='/') then x `div` y
                    else if (opt=='*') then x*y 
                    else error "wrong operator"

type H = Char
type B = [Char]
type Anda = Int

-- length :: B -> Anda
-- length [] = 0
-- length (x:xs) = 1 + length xs

--2^4
pow :: Int -> Int -> Int
pow 1 _ = 1
pow _ 0 = 1
pow n p = n*pow n (p-1) 


listpow xs p = [pow x p | x <- xs]


digit :: Int -> [Int]
digit 0 = []
digit x = digit (x `div` 10) ++ [x `mod` 10]

-- strongArm :: Int -> Bool
-- strongArm n = n == sum listpow digit length digit

-- listOfArmstrong m = [x | x <- [0..m] , strongArm x]


type Fu  = (Int ,Int ,Int)
data MakeVect = V3d Fu deriving (Show) 


makeVec :: Fu-> MakeVect 
makeVec (x ,y ,z) = V3d (x ,y ,z)





