def ints():
	for i in range(100):
		yield i
	return

def head(g):
	return next(g())

def tail(g):
	def p():
		h = g()
		next(h)
		for x in h: yield x
	return p


# foldl (+) 0 [1..5]
# z is the accumulator
# def foldl_gen(f,z,g1):
# 	def rets():
# 		yield f(z, head(g1))
# 		for x in foldl_gen(f, f(z, head(g1)), tail(g1))(): yield x
# 	return rets


# -- if the list is empty, the result is the initial value z; else
# -- apply f to the first element and the result of folding the rest
# foldr f z []     = z 
# foldr(f, z, (x:xs)) = f(x, (foldr(f, z, xs))) 

# foldr f z (x:xs) = f x (foldr f z xs)

def foldr_gen(f,z,g1):
	def rets():
		yield f(head(g1), foldr_gen(f, z, tail(g1))) 
		for x in foldr_gen(f, z, tail(g1))() : yield x
	return rets	


# def foldr_gen(f,z,g1):
# 	def rets():
# 		yield f(head(g1), foldr_gen(f, z, head(tail(g1)) ) ) 
# 		for x in foldr_gen(f, z, tail(g1))() : yield x
# 	return rets	

# def runfoldl():
# 	h = foldl_gen(lambda x,y:x+y, 0, ints)()
# 	for x in h: yield x

# i = runfoldl()
# for j in range(10): 
# 	print next(i)

def runfoldr():
	h = foldr_gen(lambda x, y: x + y, 0, ints)()
	for x in h: yield x

i = runfoldr()
for j in range(100): 
	print next(i)


let high = True:high foldr (&&) False high
