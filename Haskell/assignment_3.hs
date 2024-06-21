scalarProduct :: [Int]->[Int]->Int
scalarProduct xs ys = sum [x*y | (x,y)<-zip xs ys]
--[1,2,3] [3,2,1]
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort  [a] = [a]
mergeSort xs = merge (mergeSort (take (div (length xs) 2) xs)) (mergeSort (drop (div (length xs) 2) xs))

 


