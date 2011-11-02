# Benjamin Tan Wei Hao
# U077129N
# Problem Set 7 Ex 2

"""
	Haskell Version of zipWith series
	
	zipWith f (x:xs) (y:ys) = f x y : zipwith f xs ys
	
	< The Systematic Translation Scheme of zipWith Haskell -> Python >

	1. The above function requires 3 input parameters 
		a) function to operate on the two input lists [f]
		b) first lists	[g1]
		c) second lists [g2]

		def zipwithgen(f,g1,g2):
			def rets():
				<body>
			return rets
	
	2. <body>
	 	
	   The operation on the first element(s) :		 
	   
	   		yield f(head(g1),head(g2))        // Haskell : f x y 

	   Followed by the rest of the elements:

	  		for x in zipwith(f, tail(g1), tail(g2))() : yield x // Haskell : zipwith f xs ys

	< Using the zipWith function on Fibonacci >

	fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

	1. Code Skeleton with base cases

		def fib():
			yield 0		// Haskell ...
			yield 1     // ... 0:1
	
	2. Assign the called (zipWith) function to a variable
		
		h = zipwithgen(lambda x,y:x+y, fibs, tail(fibs))()
	
	3. Process the rest of the elemnents

		for x in h: yield x

"""
def head(g):
	return next(g())

def tail(g):
	def p():
		h = g()
		next(h)
		for x in h: yield x
	return p

def zipwithgen(f,g1,g2):
	def rets():
		yield f(head(g1),head(g2))
		for x in zipwithgen(f,tail(g1),tail(g2))(): yield x
	return rets


def fibs():
	yield 0
	yield 1
	h = zipwithgen(lambda x,y:x+y, fibs, tail(fibs))()
	for x in h: yield x


i = fibs()
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
print next(i)
