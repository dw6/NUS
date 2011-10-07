-- Write a higher order function, in any of the three languages mentioned 
-- above, that partitions a list into elements that are larger than a pivot
-- , and elements that are smaller than a pivot. The function should take
-- two arguments: the pivot, and the list to be partitioned, and return a
-- pair containing the partitions.

import List

--pivot(x,p) = partition (>p) x

--pivot (x,p) = (filter (<p) x,filter (>p) x)

pivot (x,p) = 
	let small = (filter (<p) x)
	    big = (filter (>p) x)
	in (small,big)

