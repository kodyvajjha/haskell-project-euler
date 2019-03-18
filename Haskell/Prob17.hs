import Data.Char

ones = ["one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen", "nineteen","twenty"]

twos = ["thirty","forty","fifty","sixty","seventy","eighty","ninety"]

threes = ["hundred"]

numToZiplist x = zip [1..] (map (\y -> digitToInt y) (show x))
numToZiplistRev x = zip (reverse $ map (fst) $ numToZiplist x) (map (snd) $ numToZiplist x)

myFun xs = do 
            l <- xs
            return l++[1]

myFun2 (i,j)
| `

