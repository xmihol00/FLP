import Data.List
import Data.Char
import System.IO
import System.Random

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

encode_help (x:xs)
    | xs == [] = (length x, head x):[]
    | otherwise = (length x, head x):(encode_help xs)
encode_help [] = []
encode :: Eq a => [a] -> [(Int, a)]
encode str = encode_help rev_res
    where (res, _, _) = pack_help [] [] str
          rev_res = reverse res

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified_help [] = []
encodeModified_help (x:xs)
    | xs == [] = item:[]
    | otherwise = item:(encodeModified_help xs)
    where lx = length x
          hx = head x
          item = if lx == 1 then Single hx else Multiple lx hx

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified str = encodeModified_help rev_res
    where (res, _, _) = pack_help [] [] str
          rev_res = reverse res

unroll (Multiple n x) = take n (cycle [x])
unroll (Single x) = [x]

decodeModified :: (Eq a, Enum a) => [ListItem a] -> [a]
decodeModified [] = []
decodeModified [x] = unroll x
decodeModified (x:xs) = (unroll x) ++ (decodeModified xs)

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect list@(x:xs)
    | xs == [] = encoded:[]
    | otherwise = encoded:(encodeDirect second)
    where (first, second) = span (== x) list
          length_fst = (length first)
          extracted = head first
          encoded = if length_fst == 1 then Single extracted else Multiple length_fst extracted

dupli :: [a] -> [a]
dupli [] = []
dupli (x:[]) = [x, x]
dupli (x:xs) = [x, x] ++ (dupli xs)

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:[]) n = take n (cycle [x])
repli (x:xs) n = (take n (cycle [x])) ++ (repli xs n)

drop_help [] _ _ = []
drop_help (x:[]) n y
    | y > 0 = [x]
    | otherwise = []
drop_help (x:xs) n y
    | y > 0 = x:(drop_help xs n (y-1))
    | otherwise = (drop_help xs n (n-1))

dropEvery :: [a] -> Int -> [a]
dropEvery l n = drop_help l n (n-1)

mySplit :: [a] -> Int -> ([a], [a])
mySplit [] _ = ([], [])
mySplit l 0 = ([], l)
mySplit (x:xs) 1 = ([x], xs)
mySplit (x:xs) n = (x:a, b)
    where (a, b) = mySplit xs (n-1)

mySlice :: [a] -> Int -> Int -> [a]
mySlice l x y = c
    where (_, b) = mySplit l x
          (c, _) = mySplit b (y-x)

rotate :: [a] -> Int -> [a]
rotate x n = b ++ a    
    where (a, b) = if n > 0 then mySplit x n else mySplit x ((length x) + n)

removeAt :: [a] -> Int -> (a, [a])
removeAt l n = (last a, (init a) ++ b)
    where (a, b) = mySplit l n

removeAt' :: [a] -> Int -> (a, [a])
removeAt' (x:xs) 1 = (x, xs)
removeAt' (x:xs) n = (a, x:b)
    where (a, b) = removeAt' xs (n-1)

insertAt :: [a] -> a -> Int -> [a]
insertAt l a 0 = a:l
insertAt (x:xs) y n = x:a
    where a = insertAt xs y (n-1)

range :: Int -> Int -> [Int]
range a b = [a..b]

randomInt :: Int -> Int -> IO Int
randomInt minVal maxVal = randomRIO (minVal, maxVal)

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

deduplicate :: Eq a => [a] -> [a] -> [a]
deduplicate x (y:ys)
    | elem y x = deduplicate x ys
    | otherwise = y:(deduplicate next ys)
    where next = x ++ [y]

diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
    gen <- getStdGen
    return $ take n $ deduplicate [] [x | x <- randomRs (1, m) gen]
