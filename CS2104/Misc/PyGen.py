# Python Generators

# 0. Square root of 2 approximation
def foo():
	i = 1
	while 1:
		yield i
		i = (i+2/i)/2.0


# 1. Infinite stream of increasing numbers.  

def foo():
	i = 0
	while 1:
		yield i
		i = i + 1

#2. 1 - 1/3 + 1/5 - 1/7 + ...

def foo():
	i = 1
	j = 1;
	while 1:
		yield i
		j *= -1
		i = 1.0 * j / (i + 2)

# Same series, but summed.

def pi_series():
	sum = 0
	i = 1.0; j = 1
	while(1):
		sum = sum + j/i
		yield 4*sum
		i = i + 2; j = j * -1

# 4. Infinite stream of alternating negative numbers.  

def alt_neg():
	i = 0
	j = 1
	while 1:
		yield i*j
		i = (i + 1)
		j *= -1
	
# 5. Sieve

def intsfrom(i):
	while 1:
		yield i
		i = i + 1

s = intsfrom(2)

def exclude_multiples(n, ints):
	for i in ints:
		if (i % n):
			yield i

def sieve(ints):
	while 1:
		prime = ints.next()
		yield prime
		ints = exclude_multiples(prime, ints)
