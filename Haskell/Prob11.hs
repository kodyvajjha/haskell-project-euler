main :: IO ()
main =  do mx <- readFile"/home/kody/Scripts/Haskell/ProjectEuler/Prob11file.txt"  
           print $ maximum $ leftprods mx ++ rightprods mx ++ upprods mx ++ downprods mx ++ diagleftprods mx ++ diagrightprods mx
           --print(cumleft 2 mx)
           --print(downprods mx)
nums :: String -> [[Int]]
nums a = map (map (\x -> read x :: Int) . words) $ lines a
elems :: String -> (Int, Int) -> Int
elems a (i,j) = (nums a) !! (i-1) !! (j-1)

left :: Int -> String -> [Int]
left i a = [elems a (i,j)  | j <- [1..20]]
right :: Int -> String -> [Int]
right i a = reverse $ left i a
up :: Int -> String -> [Int]
up j a = [elems a (i,j) | i <- [1..20]]
down :: Int -> String -> [Int]
down j a = reverse $ up j a 

cumleft i a 
    | i==1 = left i a
    | i>1 = cumleft (i-1) a ++ left i a

cumright i a
    | i==1 = right i a
    | i>1 = right i a ++ cumright (i-1) a 

cumup j a 
    | j==1 = up j a
    | j>1 = cumup (j-1) a ++ up j a 

cumdown j a
    | j==1 = down j a 
    | j>1 = down j a ++ cumdown (j-1) a

diagleft a = concat [ [elems a (i,j)  | i <-[1..20],j<-[1..20], i+j ==k] | k <- [2..40]] 

diagright a = map (\x -> elems a x) (concat([zip [1..k] [21-k..20] | k <- [1..20]]) ++ reverse (concat([zip [21-k..20] [1..k] | k <- [1..19]])))

leftprods a = [product $ [(cumleft 20 a) !! i, (cumleft 20 a) !! (i+1),(cumleft 20 a) !! (i+2),(cumleft 20 a) !! (i+3)] | i <- [0..396] ]

rightprods a = [product $ [(cumright 20 a) !! i, (cumright 20 a) !! (i+1),(cumright 20 a) !! (i+2),(cumright 20 a) !! (i+3)] | i <- [0..396] ]

upprods a = [product $ [(cumup 20 a) !! i, (cumup 20 a) !! (i+1),(cumup 20 a) !! (i+2),(cumup 20 a) !! (i+3)] | i <- [0..396] ]

downprods a = [product $ [(cumdown 20 a) !! i, (cumdown 20 a) !! (i+1),(cumdown 20 a) !! (i+2),(cumdown 20 a) !! (i+3)] | i <- [0..396] ]

diagleftprods a =[product $ [(diagleft a) !! i, (diagleft a) !! (i+1),(diagleft a) !! (i+2),(diagleft a) !! (i+3)] | i <- [0..396] ] 

diagrightprods a =[product $ [(diagright a) !! i, (diagright a) !! (i+1),(diagright a) !! (i+2),(diagright a) !! (i+3)] | i <- [0..396] ] 

