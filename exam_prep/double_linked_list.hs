import Data.Maybe (isJust, isNothing)

data DoubleLinkedList v = DoubleLinkedList v (Maybe (DoubleLinkedList v)) (Maybe (DoubleLinkedList v))
    deriving (Eq, Show)

isSomething :: Maybe a -> Bool
isSomething (Just _) = True
isSomething _ = False

printAllNext :: Show a => Maybe (DoubleLinkedList a) -> String
printAllNext head
    | isJust head = show value ++ ' ':printAllNext next
    | otherwise = ""
    where
        (Just justHead) = head
        (DoubleLinkedList value next _) = justHead

printAllPrevious :: Show a => Maybe (DoubleLinkedList a) -> [Char]
printAllPrevious tail
    | isJust tail = show value ++ ' ':printAllPrevious previous
    | otherwise = ""
    where
        (Just justTail) = tail
        (DoubleLinkedList value _ previous) = justTail

foldrUpdateAcc :: v -> Maybe (DoubleLinkedList v) -> Maybe (DoubleLinkedList v)
foldrUpdateAcc element accumulator
    | isNothing accumulator = Just (DoubleLinkedList element Nothing Nothing)
    | otherwise = newAcc
    where
        (Just justAcc) = accumulator
        (DoubleLinkedList accValue accPrevious accNext) = justAcc
        oldAcc = Just (DoubleLinkedList accValue accPrevious newAcc)
        newAcc = Just (DoubleLinkedList element oldAcc Nothing)

foldlUpdateAcc :: Maybe (DoubleLinkedList v) -> v -> Maybe (DoubleLinkedList v)
foldlUpdateAcc accumulator element
    | isNothing accumulator = Just (DoubleLinkedList element Nothing Nothing)
    | otherwise = newAcc
    where
        (Just justAcc) = accumulator
        (DoubleLinkedList accValue accPrevious accNext) = justAcc
        oldAcc = Just (DoubleLinkedList accValue newAcc accNext)
        newAcc = Just (DoubleLinkedList element Nothing oldAcc)

linkList :: [Maybe (DoubleLinkedList v)] -> [Maybe (DoubleLinkedList v)]
linkList [x, y] = [newJustX, newJustY]
    where
        (Just justX) = x
        (DoubleLinkedList valueX nextX previousX) = justX
        (Just justY) = y
        (DoubleLinkedList valueY nextY previousY) = justY
        newJustX = Just (DoubleLinkedList valueX newJustY previousX)
        newJustY = Just (DoubleLinkedList valueY nextY newJustX)

linkList (x:y:rest) = newJustX:linkList (newJustY:rest)
    where
        (Just justX) = x
        (DoubleLinkedList valueX nextX previousX) = justX
        (Just justY) = y
        (DoubleLinkedList valueY nextY previousY) = justY
        newJustX = Just (DoubleLinkedList valueX newJustY previousX)
        newJustY = Just (DoubleLinkedList valueY nextY newJustX)
linkList _ = []

--dllFromList :: [a] -> (Maybe (DoubleLinkedList a), Maybe (DoubleLinkedList a))
--dllFromList list = (dllHead, dllTail)
--    where
--        dllTail = foldl foldlUpdateAcc Nothing list
--        getHead tail
--            | isJust tail && isJust previous = getHead previous
--            | otherwise = tail
--            where
--                (Just justTail) = tail
--                (DoubleLinkedList value _ previous) = justTail
--        dllHead = getHead dllTail
dllFromList :: [a] -> (Maybe (DoubleLinkedList a), Maybe (DoubleLinkedList a))
dllFromList list = (dllHead, dllTail)
    where
        mapped = map (\x -> Just (DoubleLinkedList x Nothing Nothing)) list
        linked = linkList mapped
        dllHead = head linked
        dllTail = last linked


dll :: (Maybe (DoubleLinkedList Integer), Maybe (DoubleLinkedList Integer))
dll = dllFromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

dllHead :: Maybe (DoubleLinkedList Integer)
dllHead = fst dll
dllTail :: Maybe (DoubleLinkedList Integer)
dllTail = snd dll

dllThirdToLast :: Maybe (DoubleLinkedList Integer)
dllThirdToLast = thirdToLast
    where
        (Just justTail) = dllTail
        (DoubleLinkedList _ _ (Just justSecondToLast)) = justTail
        (DoubleLinkedList _ _ (Just justThirdToLast)) = justSecondToLast
        (DoubleLinkedList _ _ thirdToLast) = justThirdToLast
