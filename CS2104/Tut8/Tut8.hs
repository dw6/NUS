--Power Series

--example run
--sum $ take 1000 $ powerseries expo_term 1 [0..]
module Main where

    
    term a x pow = a * x**pow
    diff_term a x pow = pow * a * x**pow-1
    intg_term a x pow = a * x**(pow+1)/(pow+1)

    expo_term x pow = term (1/(product [1..pow])) x pow
    diff_expo_term x pow = diff_term (1/(product [1..pow])) x pow
    intg_expo_term x pow = intg_term (1/(product [1..pow])) x pow

    powerseries fn x = map (fn x) 

