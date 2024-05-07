diffFoldr :: Integer
diffFoldr = foldr (-) 0 [1 .. 10]
diffFoldl :: Integer
diffFoldl = foldl (-) 0 [1 .. 10]

myFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

myFoldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myDiffFoldr :: Integer
myDiffFoldr = myFoldr (-) 0 [1 .. 10]
myDiffFoldl :: Integer
myDiffFoldl = myFoldl (-) 0 [1 .. 10]

myConcat :: Foldable t => t a -> [a] -> [a]
myConcat xs ys = foldr (:) ys xs

myReverse :: [a] -> [a]
myReverse xs = myFoldl (\acc x -> x:acc) [] xs

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort smaller ++ (x:sort larger)
    where
        (smaller, larger) = foldr (\a (acc1, acc2) -> if a < x then (a:acc1, acc2) else (acc1, a:acc2)) ([], []) xs
