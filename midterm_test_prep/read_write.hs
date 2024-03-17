
stripTrailingSpaces line = reverse $ dropWhile (== ' ') $ reverse line

emptyLine "" = True
emptyLine _ = False

splitToLines str
    | null second = [stripTrailingSpaces first]
    | otherwise = (stripTrailingSpaces first):(splitToLines $ tail second)
    where
        (first, second) = span (/= '\n') str

removeEmptyLines = filter (not . emptyLine)
printEven (x:xs) i
    | mod i 2 == 0 = x ++ "'\n" ++ printEven xs (i+1)
    | otherwise = printEven xs (i+1)
printEven [] _ = ""

evenLines fileName = do
    content <- readFile fileName
    putStr $ printEven (removeEmptyLines $ splitToLines content) 0

evenLinesLet fileName = do
    content <- readFile fileName
    let splitted = splitToLines content
    let removedEmpty = removeEmptyLines splitted
    let finalStr = printEven removedEmpty 0
    putStr finalStr
