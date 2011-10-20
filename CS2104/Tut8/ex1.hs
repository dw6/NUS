module Main where
    expo num = foldl (\acc depth -> acc + ((num ** depth) / (product [1..depth]))) 1
   