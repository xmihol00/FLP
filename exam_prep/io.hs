
newLineSplit :: String -> [String]
newLineSplit "" = [[]]
newLineSplit ('\n':rest) = []:splitted
    where
        splitted = newLineSplit rest
newLineSplit (c:rest) = newCurrent:splitted
    where 
        current:splitted = newLineSplit rest
        newCurrent = c:current

reverseLines :: String -> String -> IO()
reverseLines inFileName outFileName = do
    fileContent <- readFile inFileName
    let splitted = newLineSplit fileContent
    let reved = map reverse splitted
    let merged = foldl (\acc str -> acc ++ ('\n':str)) "" reved
    let final = tail merged
    if outFileName == "" then putStrLn final 
                         else writeFile outFileName final
