#########################
#   Python Generators   #
#########################

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

# 2. 1 - 1/3 + 1/5 - 1/7 + ...

def foo():
	i = 1
	j = 1;
	while 1:
		yield i
		j *= -1
		i = 1.0 * j / (i + 2)

# 3. Same series, but summed.

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

###############################################################

def tail(g): # no haskell equivalent since no pattern matching
	def p():
		h = g()
		next(h)
		for x in h:
			yield x
        return p

def head(g): # no haskell equivalent since no pattern matching
    return next(g())

def mapgen(f,g):
    def rets():
        yield f(head(g))
        for x in mapgen(f,tail(g))():
        	yield x
    return rets

def zipwithgen(f,g1,g2):
    def rets():
        yield f(head(g1),head(g2))
        for x in zipwithgen(f,tail(g1),tail(g2))():
            yield x
    return rets

# Square root using mapgen
def sqroot():
	yield 1.0
	for x in mapgen(lambda x:x/2.0 + 1.0/x,sqroot)():
		yield x



def num():
	yield 0
	for x in mapgen(lambda x:x+1,num)():
		yield x

def alt():
	yield 1
	yield -1
	g=alt()
	while True:
		yield next(g) 

def stream():
	for x in zipwithgen(lambda x,y:x*y, num, alt)():
		yield x

i=stream();
k=1
while True:
    print next(i)
    k+=1
    if (k>15): break


######################
# Translation of foldl
######################

# foldl (+) 0 [1..5]
# z is the accumulator

# foldl f z (x:xs) = foldl f (f z x) xs

# Currently outputs 0, 1, 3, 6, 10, 15, 21, 28, 36, 45

def foldl_gen(f,z,g1):
	def rets():
		yield f(z, head(g1))
		for x in foldl_gen(f, f(z, head(g1)), tail(g1))(): yield x
	return rets

def runfoldl():
	h = foldl_gen(lambda x,y:x+y, 0, ints)()
	for x in h: yield x

i = runfoldl()
for j in range(10): 
	print next(i)


############
#  Haskell # 
############

# 1. Alternating negative numbers ( 0:-1:2:-3: ... )
stream = zipWith (*) [0..] alt where alt = 1:(-1):alt

# 2. Pascal's Triangle

pascal l = 1:(zipWith (\x y -> x+y) l (tail l))++[1]

# triangle l = do
# 	print l
# 	triangle (pascal l)

triangle l = l:(triangle (pascal l))


#############
#	Prolog	#
#############

% Write a Prolog procedure that takes in a list of integers, 
% and succeeds if the numbers in the list are consecutive, 
% and fails otherwise.

consec([]).
consec([_]).

consec([H1|T]) :-
   T = [H2|_],
   H2 is H1+1, 
   consec(T).  

:- consec([2,3,4,5,7]).

% Simulates scanl of Haskell in Prolog

scanl([], _, Out) :- writeln(Out).

scanl([H|T], Acc, In) :-
   Acc1 is H + Acc,
   append(In, [Acc1], Aux),
   scanl(T, Acc1, Aux).

scanl([H|T]) :- scanl([H|T], 0, []).  

:- scanl([1,2,3,4,5,6,7,8,9,10]).

#############
#	Prolog	#
#############

% 2.01 (**) Determine whether a given integer number is prime. 
is_prime(2).
is_prime(3).
is_prime(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor(P,3).  

% has_factor(N,L) :- N has an odd factor F >= L.
%    (integer, integer) (+,+)

has_factor(N,L) :- N mod L =:= 0.
has_factor(N,L) :- L * L < N, L2 is L + 2, has_factor(N,L2).

% X is the current element we are visiting.
primeList(Primes, _, _, 0):- writeln(Primes).

primeList(In, Out, X, N) :-
	(is_prime(X) -> (append(In, [X], Out), N1 is N-1) ; (Out = In, N1 is N)),
	X1 is X + 1,
	primeList(Out, _, X1, N1 ).

primes(N) :- primeList([], _, 2, N).

:- primes(100).

