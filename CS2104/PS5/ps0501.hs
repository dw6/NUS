--Benjamin Tan Wei Hao
--U077129N
--Problem Set 5 Ex 1

insert a pos list = 
    fst(
    	foldr (\x (b,c) -> 
    	(if c == pos then x:(a:b) else x:b, c+1)) 
    	([], 0) list
    )

