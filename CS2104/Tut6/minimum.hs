m :: [Integer] -> Integer
m xs = foldr1 min xs
