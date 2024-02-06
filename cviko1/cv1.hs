
idx (x:xs) (0) = x
idx (x:xs) (n) = idx xs (n-1)

elem' [] _ = False
elem' (x:xs) (y) = if x == y then True else elem' xs y 

nd [] = []
nd (x:xs) = if elem' (nd xs) x then nd xs else x : nd xs

nd_helper x [] = x
nd_helper x (y:ys) = if elem' x y then nd_helper x ys else nd_helper (reverse (y : reverse x)) ys

nd' [] = []
nd' (x:xs) = nd_helper (x:[]) (xs)

{-
nd' [] = []
nd' (x:xs) 
    | elem' xs x = nd' xs
    | otherwise = x : (nd' xs)
    -}