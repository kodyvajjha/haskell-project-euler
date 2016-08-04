
sumSq = sum $ map (\x -> x*x) [1..100]

sqSum = (\x -> x*x) $ (sum [1..100])

prob6 = sumSq - sqSum
