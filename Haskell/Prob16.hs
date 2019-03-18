import Data.Char (digitToInt)
prob_16 = sum $ map (digitToInt) (show (2^1000))
