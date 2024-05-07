import Data.Bits((.&.))
tups1 = [(a, b) | a <- [1 .. 5], b <- [8, 7 .. 0], b > a]
tups2 = [[(a, b) | a <- [1 .. 5], b > a] | b <- [8, 7 .. 0]]
multiLevel = [[a | a <- [3 .. 7], a < b || a == 7] | b <- [5, 6]]