type Meritype = [(Int,Int)]


f :: Int -> Int -> [Meritype]
f x y = [[(x,y)]] 
type Cal = (Float, Float, Char)

data Maybe2 a = Just2 a | Nothing deriving(Show)




calculator :: Integral Cal => Cal -> Float
calculator (x,y,op) = if op == '+' then x + y
                     else if op == '-' then x - y
                     else if op == '*' then x * y
                     else if op == '/' then x / y
                     else error "Wrong Operator"

type A = (Float,Float,Float)
type B = (Float,Float)
data Values2 = V3 A | V2 B deriving(Show)

data Values = Vect3d Float Float Float deriving(Show)

-- print' :: Values -> Values
-- print' (Vect3d x y z) = Vect3d x*x y*y z*z
-- data Values = Vect3d Int Int Int deriving (Show)

showVector:: Values2 -> Values2
showVector (V3 (x,y,z)) = V2 (x,y)

dotPrd :: Values2 -> Values2 -> Float
dotPrd (V3 (x,y,z)) (V3 (a,b,c)) =  x*a+y*b+z*c

dotPrd' :: Values2 -> Values2 -> Float
dotPrd' (V2 (x,y)) (V2 (a,b)) =  x*a+y*b

crossPrd' :: Values2 -> Values2 -> Values2
crossPrd' (V2 (x,y)) (V2 (a,b)) =  V2 (x*a ,y*b)

data Shape a = Circle a | Rect a a deriving(Show)
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving(Show)

fuckC :: Float -> Shape Float
fuckC x = Circle (x*69)
instance Functor Shape where 
    fmap func (Circle r) = Circle (func r)
    fmap func (Rect w h) = Rect (func w) (func h)
-- 
instance Applicative Shape where
    pure a = Circle a
    Circle func <*> Circle r = Circle (func r)
instance Monad Shape where
    (Circle r) >>= f2 = f2 r

instance Functor Tree where
    fmap func (Leaf a) = Leaf (func a)
    fmap func (Branch a b) = Branch (fmap func a) (fmap func b)

instance Applicative Tree where
    pure a = Leaf a
    Leaf func <*> Leaf r = Leaf (func r)
    Branch a b <*> func = Branch (a <*> func) (b <*> func)



-- area :: Shape a -> Float
-- area (Circle r ) = r*r * 3.14 


--area' :: Shape -> Float
--area' (Rect x y) = x*y



