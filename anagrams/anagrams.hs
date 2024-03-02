import Data.List (permutations)
import Data.Char (toLower)

anagramsFilter :: [String] -> [String] -> [String]
anagramsFilter [] _ = []
anagramsFilter (x:xs) xss
    | elem (map toLower x) (map (map toLower) xss) && notElem (map toLower x) (map (map toLower) xs) = x : anagramsFilter xs xss
    | otherwise = anagramsFilter xs xss

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = anagramsFilter (permutations xs) (filter (\x -> (map toLower x) /= (map toLower xs)) xss)
