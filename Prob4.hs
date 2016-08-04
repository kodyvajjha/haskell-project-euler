isPalindrome xs = reverse(show xs) == show xs

threeProds = [i*j | i<-[100..999],j<-[100..999]]

problem_4 = maximum(filter (\n -> isPalindrome(n) == True) threeProds)
