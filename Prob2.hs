fibs :: Int -> Int

fibs 1 = 1
fibs n = if n == 2
         then 1
         else fibs(n-1) + fibs(n-2)

problem_2 = sum(takeWhile (<4000000)(map fibs (map (*2) [1..])))
