import Data.Maybe (isJust, isNothing)

data DLL a = DLL { previous :: Maybe (DLL a), value :: a, next :: Maybe (DLL a) }

nextShow :: (Show a) => DLL a -> String
nextShow (DLL _ v Nothing) = show v
nextShow (DLL _ v (Just n)) = show v ++ " -> " ++ nextShow n

previousShow :: (Show a) => DLL a -> String
previousShow (DLL Nothing v _) = show v
previousShow (DLL (Just p) v _) = show v ++ " -> " ++ previousShow p

dllShow :: (Show a) => (Maybe (DLL a), Maybe (DLL a)) -> String
dllShow (head, tail)
    | isNothing head || isNothing tail = "Empty"
    | otherwise = "head to tail: " ++ nextShow h ++ "\ntail to head: " ++ previousShow t
    where 
        Just h = head
        Just t = tail

dllFromList :: [a] -> (Maybe (DLL a), Maybe (DLL a))
dllFromList [] = (Nothing, Nothing)
dllFromList [x] = (firstNode, firstNode)
    where
        firstNode = Just (DLL Nothing x Nothing)
dllFromList (x:y:xs) = (Just firstNode, Just lastNode)
    where
        firstNode = DLL Nothing x (Just secondNode)
        (secondNode, lastNode) = buildDLL firstNode y xs

buildDLL :: DLL a -> a -> [a] -> (DLL a, DLL a)
buildDLL previousNode value [] = (node, node)
    where 
        node = DLL (Just previousNode) value Nothing
buildDLL previousNode value (x:xs) = (node, lastNode)
    where
        node = DLL (Just previousNode) value (Just nextNode)
        (nextNode, lastNode) = buildDLL node x xs

dll :: (Maybe (DLL Integer), Maybe (DLL Integer))
dll = dllFromList [1 .. 10]

dllHead :: DLL Integer
dllHead = node
    where
        Just node = fst dll
dllTail :: DLL Integer
dllTail = node
    where
        Just node = snd dll

dllPrint :: IO ()
dllPrint = putStrLn $ dllShow dll
