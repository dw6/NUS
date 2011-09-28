# Compute the sum of a list
l = [1,2,3,4,5]

def sum(list):
	total = 0
	for l in list:
		total += l
	return total

print sum(l)