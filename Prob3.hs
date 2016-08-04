#! /usr/bin/env runhugs +l
--
-- Prob3.hs
-- Copyright (C) 2016 kody <kody@kodyarch>
--
-- Distributed under terms of the MIT license.
--

factor :: Int -> Int -> [Int]
factor n x = filter (\n -> not(n `mod` x == 0) ) [2..n]

primes = [p | p<-[2..], product[1..p-1] `rem` p == p-1]

