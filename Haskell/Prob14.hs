collFun :: Int -> Int
collFun n 
    | n `rem` 2 == 0 = n`div`2
    | n `rem` 2 == 1 = 3*n + 1

lenChain n = (+1).length $ takeWhile (\y -> y /= 1) $ iterate (collFun) n

-- Using the above, we get that 
-- maximum $ map (lenChain) [1..100000] equals 525

-- So.
prob_14 = filter (\x -> lenChain x == 525) [1..1000000]

-- Answer is 837799.
