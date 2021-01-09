mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (take halfLen xs)) (mergeSort (drop halfLen xs))
  where
    halfLen = length xs `quot` 2
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | x <= y = x : y : merge xs ys
      | otherwise = y : x : merge xs ys