import Ratio
power x = zipWith (/) (map (x^) [0..10]) (map (\x -> product [1..x]) [0..10]) :: Ratio