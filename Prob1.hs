problem_1 = sumStep 3 999 + sumStep 5 999 - sumStep 15 999
  where
      sumStep s n = s * sumOnetoN (n `div` s)
      sumOnetoN n = n * (n+1) `div` 2
