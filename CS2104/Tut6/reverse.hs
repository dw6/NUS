module Main where
	rev [] = []
	rev (x:xs) = rev(xs) ++ [x] 