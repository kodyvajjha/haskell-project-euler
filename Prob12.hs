import Data.Numbers.Primes
import Data.List

triangleNum :: Integral a => a -> a
triangleNum n = n*(n-1) `div` 2

findFactor :: Integral a => a -> [a]
findFactor x = filter (\k -> x `rem` k == 0) [1..x]

triangleNums :: Integral a => [a]
triangleNums = map (triangleNum) [1..]

divFun :: Int -> Int
divFun x = length $ findFactor x


divsofTriang = map (divFun) triangleNums

-- does not halt :(
-- giving up

--problem_12 = head $ filter (\k -> divFun k > 150) triangleNums

-- copying the solution from the haskell project euler page

primeFactors n = factor n primes
  where
      factor n (p:ps) 
          | p*p > n        = [n]
          | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
          | otherwise      =     factor n ps

problem_12 = head $ filter ((> 500) . nDivisors) triangleNumbers
  where nDivisors n = product $ map ((+1) . length) (group (primeFactors n))    
        triangleNumbers = scanl1 (+) [1..]
