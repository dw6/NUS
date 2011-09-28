# Write a Python program that converts a dictionary into a list. 
# All the keys and values in the dictionary should appear in the result list in some order.

def dic2list(input):
	list = []
	for k,v in input.iteritems() :	
		list.append([k,v])
	return list

print dic2list({"terror" : "win", "counter" : "lose"})