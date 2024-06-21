merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x > y then y : merge (x:xs) ys else x : merge xs (y : ys)

