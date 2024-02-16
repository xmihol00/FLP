import Data.List
import Data.Char
import System.IO

compress :: Eq a => [a] -> [a]
compress (x:y:xs)
    | x == y = compress (y:xs)
    | otherwise = x:(compress (y:xs))
compress (x:[]) = x:[]
compress _ = []

pack_help :: Eq a => [[a]] -> [a] -> [a] -> ([[a]], [a], [a])
pack_help f p (x:y:xys)
    | x == y = pack_help f (x:p) (y:xys)
    | null p = pack_help ([x]:f) [] (y:xys)
    | head p == x = pack_help ((x:p):f) [] (y:xys)
    | otherwise = pack_help ([x]:p:f) [] (y:xys)
pack_help f p (x:[])
    | null p = (([x]:f), [], [])
    | head p == x = (((x:p):f), [], [])
    | otherwise = (([x]:p:f), [], [])
pack_help f [] [] = (f, [], [])

pack :: Eq a => [a] -> [[a]]
pack str = reverse res
    where (res, _, _) = pack_help [] [] str

