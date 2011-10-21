declare
fun {Ints N Max}
   if N == Max then nil
   else 
      N|{Ints N+1 Max}
   end
end

fun {Sieve List}
   case List
   of nil then nil
   [] X|Xs then Ys in
      Ys = {Filter Xs fun {$ Y} Y mod X \= 0 end}
      X|{Sieve Ys}
   end
end
{Browse {Sieve {Ints 2 100}}}