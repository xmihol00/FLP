module Parsing (
    isRight, 
    takeRestOfText, 
    skipWhiteSpaces, 
    skipWhiteSpacesAndNewLines, 
    formattedError, 
    takeErrorSample, 
    utf8Print,
    trim,
    split,
    checkRemoveIndent,
    decimalOrError,
    doubleOrError,
    parseValues
) where
import qualified Data.Char as C
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Read as TL

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

takeRestOfText :: String -> (String, String)
takeRestOfText = span (\x -> x /= '\n' && x /= ' ' && x /= '\t')

skipWhiteSpaces :: String -> String
skipWhiteSpaces = dropWhile (\x -> x == ' ' || x == '\t')

skipWhiteSpacesAndNewLines :: String -> String
skipWhiteSpacesAndNewLines = dropWhile C.isSpace

takeErrorSample :: String -> String
takeErrorSample = takeWhile (/= '\n') . take 16

formattedError :: (Show a1, Num a1) => a1 -> String -> a2
formattedError line text = error ("\nLine " ++ show (line + 1) ++ ": " ++ text ++ "\n")

utf8Print :: String -> IO ()
utf8Print = T.putStrLn . T.pack

split :: Char -> String -> [String]
split delimiter strData
    | null strData || all ((==True) . C.isSpace) strData = []
    | otherwise = start : split delimiter rest
    where (start, nlRest) = span (/= delimiter) strData
          rest
            | null nlRest = []
            | otherwise = tail nlRest

trim :: String -> String
trim str = trimEnd
    where trimmedStart = dropWhile C.isSpace str
          trimEnd = reverse $ dropWhile C.isSpace $ reverse trimmedStart

checkRemoveIndent :: String -> Int -> String
checkRemoveIndent input line = trimmed
    where (spaces, trimmedUnchecked) = splitAt (2 * line) input
          check = all (==' ') spaces || formattedError line ("Wrong indent at line " ++ show line ++ ", unexpected indent.")
          trimmed = if check then trimmedUnchecked else ""

decimalOrError :: String -> Int -> (Int, String)
decimalOrError input line = (num, rest)
    where numMaybe = TL.decimal $ TL.pack $ skipWhiteSpaces input
          (Right (num, restPacked)) = if isRight numMaybe then numMaybe else formattedError line $ "Unable to parse '" ++ takeErrorSample input ++ "...' as a natural number."
          rest = TL.unpack restPacked

doubleOrError :: String -> Int -> (Double, String)
doubleOrError input line = (num, rest)
    where numMaybe = TL.double $ TL.pack $ skipWhiteSpaces input
          (Right (num, restPacked)) = if isRight numMaybe then numMaybe else formattedError line $ "Unable to parse '" ++ takeErrorSample input ++ "...' as a floating point number."
          rest = TL.unpack restPacked

parseValuesLine :: String -> Int -> ([Double], String)
parseValuesLine str line
    | end = ([value], restSpaces2)
    | otherwise = (value:values, rest)
    where (value, restDouble) = doubleOrError str line
          restSpaces1 = skipWhiteSpaces restDouble
          restComma
            | null restSpaces1 || head restSpaces1 == '\n' = restSpaces1
            | head restSpaces1 == ',' = tail restSpaces1
            | otherwise = formattedError line $ "Comma expected at: '" ++ takeErrorSample restSpaces1 ++ "...'"
          restSpaces2 = skipWhiteSpaces restComma
          end = null restSpaces2 || head restSpaces2 == '\n'
          (values, rest) = parseValuesLine restSpaces2 $ line + 1

parseValues :: String -> Int -> ([[Double]], String)
parseValues str line
    | null $ skipWhiteSpacesAndNewLines str = ([], [])
    | otherwise = (parsedRow:parsedRows, restLines)
    where (parsedRow, restLine) = parseValuesLine str line
          nextRow
            | null restLine = restLine
            | head restLine == '\n' = tail restLine
            | otherwise = formattedError line $ "Unexpected character at: '" ++ takeErrorSample restLine ++ "...'"
          (parsedRows, restLines) = parseValues nextRow $ line + 1
          