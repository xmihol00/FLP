
dropN i [] = []
dropN i str@(x:xs)
    | i <= 0 = str
    | otherwise = dropN (i-1) xs

takeN i [] = []
takeN i (x:xs)
    | i <= 0 = []
    | otherwise = x:(takeN (i-1) xs)

charReplace _ _ _ [] = []
charReplace i c1 c2 (x:xs)
    | not (null end) && headEnd == c1 = (start ++ (c2:tailEnd)):(charReplace i c1 c2 xs)
    | otherwise = x:(charReplace i c1 c2 xs)
    where 
        start = takeN i x
        end = dropN i x
        headEnd = head end
        tailEnd = tail end
