
maybeApply :: Maybe a -> (a -> b) -> Maybe b
maybeApply Nothing _ = Nothing
maybeApply (Just x) f = Just (f x)

maybeFind :: Eq a => [(a, b)] -> a -> Maybe (a, b)
maybeFind [] _ = Nothing
maybeFind (e@(a, b):xs) x
    | a == x = Just e
    | otherwise = maybeFind xs x

maybeInsertAt :: [b] -> b -> Int -> Maybe [b]
maybeInsertAt list x 0 = Just (x:list)
maybeInsertAt [] _ _ = Nothing
maybeInsertAt (a:as) x n = maybeApply (maybeInsertAt as x (n-1)) (a:)
