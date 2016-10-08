main :: IO ()
main = do
       mx <- readFile "/home/kody/Scripts/Haskell/ProjectEuler/Prob13file.txt"
       print(take 10 (show(sum(map (\x-> read x :: Integer) (lines $ mx)))))

 
