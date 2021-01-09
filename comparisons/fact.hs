fact :: Int -> Integer
fact n = foldl (*) 1 [2..n]