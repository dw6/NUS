hamming = 1 : 
	map (2*) hamming 
		`merge` 
	map (3*) hamming 
		`merge` 
	map (5*) hamming
	where	
	merge (x:xs) (y:ys)
		| x < y = x:xs `merge` (y:ys)
		| x > y = x:xs `merge` (y:ys)
		| otherwise = x:xs `merge` ys