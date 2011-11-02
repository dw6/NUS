# Benjamin Tan Wei Hao
# U077129N
# Problem Set 7 Ex 2

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

# Haskell Version of Fibonnaci series
# fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
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

