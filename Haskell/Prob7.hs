import Data.Numbers.Primes
import Data.List
-- Somehow this doesn't work.
p n = n*log(n) + n*log(log(n)) - n + (n*log(log(n))-2*n)/log(n) - (n*(log(log(n)))^2 - 6*n*log(log(n)) + 11*n)/2*(log(n))^2

-- Bounds for the nth prime. 
a n = n*log(n) + n*(log(log(n))-1)
b n = n*log(n) + n*log(log(n))

-- We sieve within this range now. 

p `prim` xs = filter (\x -> not(x`rem`p==0)) xs

-- Couldn't sieve so giving up.


problem_7 = primes !! 10000
