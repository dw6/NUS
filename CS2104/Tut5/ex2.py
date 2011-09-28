# Write a Python program that creates a dictionary from a list. 
# Each even-ranked element of the list will become a key of the dictionary. 
# Each odd-ranked element will become the value associated to the key represented 
# by the preceding element in the list.

def list2dic(input):
	dict = {}
	for i in xrange(0, len(input), 2) : 
		dict[input[i+1]] = input[i]
	return dict

print list2dic(["test", '1', 2, 'Five', 'Preacher', 'cold'])