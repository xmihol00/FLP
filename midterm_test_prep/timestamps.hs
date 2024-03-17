
mySpan str delimiter
    | null str = ([], [])
    | head str == delimiter = ([], str)
    | otherwise = ((head str):first, second)
    where (first, second) = mySpan (tail str) delimiter


splitColon str
    | null second = [first]
    | otherwise = first:(splitColon $ tail second)
    where (first, second) = mySpan str ':'
