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
takeRestOfLine = span (\x -> x /= '\n')

skipWhiteSpaces :: String -> String
skipWhiteSpaces = dropWhile (\x -> C.isSpace x && x /= '\n')

skipWhiteSpacesAndNewLines :: String -> String
skipWhiteSpacesAndNewLines = dropWhile C.isSpace

takeErrorSample :: String -> String
takeErrorSample = takeWhile (/= '\n') . take 16

formattedError :: (Show a) => Int -> String -> a
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

checkRemoveIndent :: String -> Int -> Int -> String
checkRemoveIndent input indent line = trimmed
    where (spaces, trimmedUnchecked) = splitAt (2 * indent) input
          trimmed
            | not $ all (==' ') spaces = formattedError line ("Unexpected indent.")
            | otherwise = trimmedUnchecked

numberOrError :: String -> (TL.Text -> Either a1 (a2, TL.Text)) -> Either a1 (a2, TL.Text) -> (a2, String)
numberOrError input parser maybeError = (num, rest)
    where numMaybe = parser $ TL.pack $ skipWhiteSpaces input
          (Right (num, restPacked))
            | isRight numMaybe = numMaybe
            | otherwise = maybeError
          rest = TL.unpack restPacked

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
