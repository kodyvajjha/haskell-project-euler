import Data.Numbers.Primes

problem_10 =sum $ takeWhile (< 2000000) primes
