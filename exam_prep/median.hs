sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:xs) = sortedLeft ++ x:sortedRight
    where
        left = foldr (\y acc -> if y < x then y:acc else acc) [] xs
        right = foldl (\acc y -> if y >= x then y:acc else acc) [] xs
        sortedLeft = sort left
        sortedRight = sort right

len :: Foldable t => t a -> Int
len = foldl (\acc _ -> acc + 1) 0 

discardN :: Int -> [a] -> [a]
discardN _ [] = []
discardN 0 rest = rest
discardN n (x:xs)
    | n < 0 = error "Negative value entered"
    | otherwise = discardN (n - 1) xs

mid :: Ord a => [a] -> a
mid [] = error "Empty list"
mid xs = median
    where
        sorted = sort xs
        halfLen = div (len sorted) 2
        discarded = discardN halfLen sorted
        (median:_) = discarded
