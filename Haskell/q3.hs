-- merge :: Ord a => [a] -> [a] -> [a]
-- merge [] [] = []
-- merge [] ys = ys
-- merge xs [] = xs
-- merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
--                     | otherwise = y : merge (x:xs) ys

-- mergeSort :: Ord a => [a] -> [a]
-- mergeSort [] = []
-- mergeSort  [a] = [a]
-- mergeSort xs = merge (mergeSort (take (div (length xs) 2) xs)) (mergeSort (drop (div (length xs) 2) xs))

map f = foldr (\x xs  -> f x : xs) []
filter f = foldr (\x xs  -> if f x then x : xs else xs) []

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

mersege (msrot a msort b)


quick :: Ord a => [a] -> [a]
quick [] = []
quick (x:xs) = quick ys ++ [x] ++ quick zs
               where
               ys = [a|a <- xs , a <= x] 
               zs = [a|a <- xs , a > x] 

ini :: Foldable t => t a1 -> [a2] -> [a2]
ini xs = drop (length (xs)-1)