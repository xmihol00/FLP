
mySpan f [] = ([], [])
mySpan f str@(x:xs)
    | f x = (x:first, second)
    | otherwise = ([], str)
    where
        (first, second) = mySpan f xs

mySplit delim str
    | null second = [first]
    | otherwise = first:(mySplit delim tailSecond)
    where
        (first, second) = mySpan (/= delim) str
        tailSecond = tail second

myMap f [] = []
myMap f (x:xs) = (f x):(myMap f xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs)
    | f x = x:myFilter f xs
    | otherwise = myFilter f xs

myReduce f acc [] = acc
myReduce f acc (x:xs) = f (myReduce f acc xs) x

data TimeStamp = TimeStamp { hours :: Int, minutes :: Int, seconds :: Int }

isValid (TimeStamp h m s) = h >= 0 && h < 24 && m >= 0 && m < 60 && s >= 0 && s < 60

parseTimeStamp :: String -> TimeStamp
parseTimeStamp [h1, h2, ':', m1, m2, ':', s1, s2] = TimeStamp (readInt h) (readInt m) (readInt s)
    where
        h = [h1, h2]
        m = [m1, m2]
        s = [s1, s2]
        readInt = \x -> read x :: Int
parseTimeStamp _ = error "Invalid time stamp."

timeStampToStr :: TimeStamp -> String
timeStampToStr (TimeStamp h m s) = hPadded ++ ":" ++ mPadded ++ ":" ++ sPadded
    where
        hPadded = if h < 10 then '0':(show h) else (show h)
        mPadded = (if m < 10 then "0" else "") ++ show m
        sPadded = (if s < 10 then "0" else "") ++ show s

findInvalidTimeStamps fileName = do
    content <- readFile fileName
    let lines = mySplit '\n' content
    let timeStamps = zip (myMap parseTimeStamp lines) [1, 2 ..]
    let invalidTimeStamps = myFilter (\pair@(x, y) -> not (isValid x)) timeStamps
    let strs = myMap (\pair@(x, y) -> "line " ++ (show y) ++ ": " ++ timeStampToStr x) invalidTimeStamps
    putStr $ myReduce (\acc x -> x ++ "\n" ++ acc ) "" strs