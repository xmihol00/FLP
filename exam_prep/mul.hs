mul a b
    | b == 0 = 0
    | otherwise = a + mul a (b - 1)