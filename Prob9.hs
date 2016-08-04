-- This does the job.
pytrips = [(a,b,c,d) | a <- [1..200],b<-[1..500],c<-[1..500],let d=a+b+c, a<b,b<c, a*a + b*b == c*c,d==1000]

