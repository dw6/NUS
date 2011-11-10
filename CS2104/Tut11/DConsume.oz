% Producer Driven.
% Consumer will wait for the Producer
fun {Produce N Limit}
	if N<Limit then
	N | {Produce N+1 Limit} 
	else nil end
end

fun {Consume Xs Acc}
	case Xs of X|Xr then
		{Consume Xr Acc+X}
	[] nil then Acc
	end
end

% Assuming the consumer thread runs first, it will block on "Xs"
since Xs is currently unbounded.

The producer thread then can unblock and produce N|_
Consume thread then can be unblocked since Xs is a list (N|_) and
X has a value. Acc+X can also carry on.

% Consumer Driven.
% Producer will waiting for the Consumer

fun {DConsume ?Xs A Limit}
	if Limit > 0 then 
		local X Xr in
			Xs = X|Xr {DConsume Xr A+X Limit-1}	
	else A end
end

proc {DProduce N Xs}
	case Xs of X|Xr then
		X=N
		{DProduce N+1 Xr}
	end
end

local Xs S in
	thread {DProduce 0 Xs} end
	thread S={DConsume Xs 0 10000} end
end

% Assuming DConsume threads run first :

% Then because "X" is unbounded, the thread will be blocked on "A+X" 
% In this case, Xs = _|_, so Xs is a list.

% DProduce becomes unblocked (if previously blocked), because Xs can
% be determined to be a list. 

% When "X=N" occurs, DConsume becomes unblocked, because A+X can proceed.

% A recursive call to DProduce will again block on Xs (Xr=Xs) because 
% Xs is unbounded.

% Now, DConsume recursively calls {DConsume _ A+X Limit-1 }. 
% Then Xs = _|_, so since Xs is a list, DProduce is unblocked and can proceed.

