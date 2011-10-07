ex (a, b, 0) = (a,b)
ex (a, b, n)  
    | even a = ex (a*2, 6*a+1, n-1)
    | otherwise = ex (2*b+1, b-1, n-1)

