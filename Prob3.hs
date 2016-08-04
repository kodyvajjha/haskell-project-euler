findFactor x = filter (\k -> x `rem` k == 0) [1..x]

isPrime n
    | (product [1..n-1] + 1) `rem` n == 0 = 1
    | otherwise = 0

problem_3 = filter (\n -> isPrime(n) == 1)(findFactor(600851475143))
