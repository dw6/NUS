-- Benjamin Tan Wei Hao
-- U077129N
-- Problem Set 7 Ex 1

module Sim where

-- the signal that is always on "High"
high::[Bool]
high = True:high

-- the signal that is always on "Low"
low::[Bool]
low = False:low

-- create a limited list, of length "n", filled with logical value "fill"
-- useful to create signals by appending finite sequences
set :: Integer -> Bool -> [Bool]
set 0 _ = []
set n fill | n>0 = fill:(set (n-1) fill)

-- delay a signal by "n" clock cycles
--   prepends "n" instances of "fill" to the signal
delay :: Integer -> Bool -> [Bool] -> [Bool]
delay n fill s = (set n fill) ++ s

-- the inverter gate inverts every level for the entire signal,
-- and also delays the signal by one clock cycle
not_gate :: [Bool] -> [Bool]
not_gate s = delay 1 True (map not s)


clock :: [Bool]
clock = not_gate clock

-- and gate delays its output by 2 clock cycles
and_gate :: [Bool] -> [Bool] -> [Bool]
and_gate i1 i2 = delay 2 True (zipWith (&&) i1 i2)
                              
nand_gate :: [Bool] -> [Bool] -> [Bool] -> [Bool]
nand_gate i1 i2 i3 = not_gate(and_gate (and_gate i1 i2) (i3))

jk_flip_flop :: [Bool]->[Bool]->[Bool]->[Bool]
jk_flip_flop j k clock = 
	let (q,qbar,w1,w2) = 
		(nand_gate w1 qbar high, nand_gate w2 q high, nand_gate j clock qbar, nand_gate k clock q) in q

--sync_counter :: [Bool] -> [Bool] -> [Bool] -> ([Bool])
sync_counter j1 k1 clock = 
	let(q1,q2,w1,q3,w2,q4,o1,o2,o3,o4) = 
		(jk_flip_flop j1 k1 clock, jk_flip_flop q1 q1 clock, and_gate q1 q2, jk_flip_flop w1 w1 clock,
		and_gate w1 q3, jk_flip_flop w2 w2 clock, q4, q3, q2, q1) in (o1, o2, o3, o4)

  
--
--  take 40 (srneg_latch ((set 6 False)++high) (set 15 True)++(set 6 False)++high)
