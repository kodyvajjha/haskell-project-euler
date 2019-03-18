
evenDiv x n = x `rem` n == 0

-- Extremely inefficient. Program doesn't halt!
compareTruths y = map (evenDiv y) [1..13] == [ x==x | x <- [1..13]]

req = [y | y<-[1..],compareTruths y == True]

-- Need to find the LCM of 1..20.
-- Note that lcm(a,b,c) = lcm(a,lcm(b,c))

multLCM xs 
    | length(xs) == 2 = lcm  (head xs) (last xs)
    | length(xs) > 2 = lcm  (head xs) (multLCM(drop 1 xs))


