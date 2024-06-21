--Not User defined

module Vector (dotproduct, crossproduct) where

dotproduct :: (Float, Float, Float) -> (Float, Float, Float) -> Float 
deriving (Show)

dotproduct (l, m , n) (x, y, z) = l*x+m*y+n*z



crossproduct :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
crossproduct (l, m , n) (x, y, z) = (l*x, m*y, n*z)

--Using User defined

data Vect = THDVect Float Float Float | TWDVect Float Float deriving (Show)

make3DVect :: Float->Float->Float->Vect
make3DVect l m n = THDVect l m n

dotproduct1 :: Vect->Vect->Float
dotproduct1 (THDVect l m n) (THDVect x y z) = l*x+m*y+n*z

crossproduct1 :: Vect->Vect->Vect
crossproduct1 (THDVect l m n) (THDVect x y z) = THDVect l*x m*y n*z

