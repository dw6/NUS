--Benjamin Tan Wei Hao
--U077129N
--Problem Set 5, Exercise 2

count x l = foldr (+) 0 [ 1 | y <- l, x==y ]