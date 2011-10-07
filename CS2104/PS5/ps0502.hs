count x l = foldr (+) 0 [ 1 | y <- l, x==y ]


