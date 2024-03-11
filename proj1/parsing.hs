module Parsing (
    isRight,
    takeRestOfLine,
    skipWhiteSpaces,
    skipWhiteSpacesAndNewLines,
    formattedError,
    takeErrorSample,
    utf8Print,
    trim,
    split,
    checkRemoveIndent,
    integerOrError,
    integerOrLineError,
    doubleOrError,
    doubleOrLineError,
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

takeRestOfLine :: String -> (String, String)
takeRestOfLine = span (/= '\n')

skipWhiteSpaces :: String -> String
-- skips all white spaces apart from new lines
skipWhiteSpaces = dropWhile (\x -> C.isSpace x && x /= '\n')

skipWhiteSpacesAndNewLines :: String -> String
-- skips all white spaces including new lines
skipWhiteSpacesAndNewLines = dropWhile C.isSpace

takeErrorSample :: String -> String
-- take just couple of characters on the line where error was located
takeErrorSample = takeWhile (/= '\n') . take 16

formattedError :: (Show a) => Int -> String -> a
-- print the error in a reasonable format
formattedError line text = error ("\nLine " ++ show (line + 1) ++ ": " ++ text ++ "\n")

utf8Print :: String -> IO ()
-- make sure UTF-8 characters are printed properly
utf8Print = T.putStrLn . T.pack

split :: Char -> String -> [String]
split delimiter strData
    -- no more characters or the rest of the characters are white spaces
    | null strData || all ((==True) . C.isSpace) strData = []
    -- find the first delimiter and call itself recursively
    | otherwise = start : split delimiter rest
    where (start, nlRest) = span (/= delimiter) strData
          rest
            | null nlRest = []
            | otherwise = tail nlRest -- remove the delimiter from the 'nlRest'

trim :: String -> String
trim str = trimEnd
    where
        -- trim from the beginning
        trimmedStart = dropWhile C.isSpace str
        -- trim from the back (by reversing trimming from the beginning and reversing back)
        trimEnd = reverse $ dropWhile C.isSpace $ reverse trimmedStart

checkRemoveIndent :: String -> Int -> Int -> String
checkRemoveIndent input indent line = trimmed
    where 
        -- separate to the expected indent and the rest of the input
        (spaces, trimmedUnchecked) = splitAt (2 * indent) input
        trimmed
          | not $ all (==' ') spaces = formattedError line "Unexpected indent." -- wrong number of spaces
          | otherwise = trimmedUnchecked

numberOrError :: String -> (TL.Text -> Either a1 (a2, TL.Text)) -> Either a1 (a2, TL.Text) -> (a2, String)
numberOrError input parser maybeError = (num, rest)
    where 
        -- first remove leading white spaces, second convert to different textual representation, third parse the input
        numMaybe = parser $ TL.pack $ skipWhiteSpaces input
        -- fourth check the result of parsing, unpack it if successful, otherwise produce an error
        (Right (num, restPacked))
          | isRight numMaybe = numMaybe
          | otherwise = maybeError
         -- convert the rest if the input back to String
        rest = TL.unpack restPacked

-- define conversions to specific data types
integerOrError :: Integral a => String -> Either String (a, TL.Text) -> (a, String)
integerOrError input = numberOrError input TL.decimal

integerOrLineError :: String -> Int -> (Int, String)
integerOrLineError input line = integerOrError input (formattedError line $ "Unable to parse '" ++ takeErrorSample input ++ "...' as a natural number.")

doubleOrError :: String -> Either String (Double, TL.Text) -> (Double, String)
doubleOrError input = numberOrError input TL.double

doubleOrLineError :: String -> Int -> (Double, String)
doubleOrLineError input line = doubleOrError input (formattedError line $ "Unable to parse '" ++ takeErrorSample input ++ "...' as a floating point number.")

parseValuesLine :: String -> Int -> ([Double], String)
parseValuesLine str line
    | end = ([value], restSpaces2)
    | otherwise = (value:values, rest)
    where (value, restDouble) = doubleOrLineError str line
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
