--Power Series
module Main where
    --expo num = foldl (\acc depth -> acc + ((num ** depth) / (product [1..depth]))) 1
   --This defines one term of an exponential power series function 
   expo num = map (\depth -> ((num ** depth) / (product [1..depth]))) 
   

--Write a Haskell function that takes in an infinite stream that represents a power series, 
--and a value for variable x, and computes an infinite stream of approximations to the value 
--of the series for the given argument. Test your function on the stream defined in your 
--solution to Exercise 1.