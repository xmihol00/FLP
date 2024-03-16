data Expr = Add Expr Expr | Const Int | Var String
    deriving (Show)

tree :: Expr
tree = Add (Add (Const 1) (Add (Const 2) (Const 8))) (Add (Add (Const 4) (Const 3)) (Var "Ahoj"))

simplify :: Expr -> Expr
simplify (Add (Const x) (Const y)) = Const (x+y)
simplify (Add x y)
    | test && isConst left && isConst right = Const (l+r)
    | otherwise = Add right left
    where
        right = simplify x
        left  = simplify y
        isConst = \x -> case x of
                             (Const _) -> True
                             _ -> False
        test = case right of
                (Const _) -> True
                _ -> False
        (Const l) = left  -- vyuziti lazy evaluace, nestanes se, pokud se nepouzije promenna l
        (Const r) = right
simplify x = x

data BoolExpr = And BoolExpr BoolExpr | 
                Not BoolExpr |  -- !!!
                VarB String
    deriving (Show)

boolTree :: BoolExpr
boolTree = And (Not (Not (Not (Not (VarB "LOL"))))) (And (Not (Not (VarB ":D"))) (Not (Not (Not (VarB ":(")))))

em :: BoolExpr -> BoolExpr
em (Not (Not x)) = em x
em (And x y) = And (em x) (em y)
em (Not x) = Not (em x) -- !!!!
em x = x -- this covers (Var x)

data SearchTree k a = Node k a (SearchTree k a) (SearchTree k a) | Empty -- pozor, definice parametrickeho datoveho typu

searchTree :: SearchTree Int String
searchTree = Node 1 "one" (Node 2 "two" (Node 1 "I" Empty (Node 1 "0b1" Empty Empty)) (Node 2 "0b10" Empty (Node 2 "II" Empty Empty))) (Node 1 "0x1" (Node 2 "--" Empty Empty) Empty)

find :: Ord k => SearchTree k a -> k -> [a] -- vrati vsechny vyskyty (pokud klic neexistuje, vrati prazdny seznam)
find Empty _ = []
find (Node id value left right) key
    | id == key = value:rest
    |otherwise = rest
    where 
        rest = (find left key) ++ (find right key)

data BinTree k a = BinNode k a (BinTree k a) (BinTree k a) | BinEmpty

binTree :: BinTree Int String
binTree = BinNode 1 "one" (BinNode (-2) "minus two" (BinNode 1 "I" (BinNode (-4) "minus four" BinEmpty BinEmpty) BinEmpty) (BinNode 2 "0b10" BinEmpty (BinNode 2 "II" BinEmpty BinEmpty))) (BinNode 8 "eight" (BinNode 5 "five" BinEmpty BinEmpty) BinEmpty)
    
instance (Show k, Show v) => Show (BinTree k v) where
    show tree = showTree tree 0

showTree :: (Show k, Show v) => BinTree k v -> Int -> String
showTree (BinNode k v l r) indent = thisNode ++ leftNode ++ rightNode
    where 
        thisNode = (take indent [' ', ' ' ..]) ++ "Node: " ++ show k ++ " " ++ show v ++ "\n"
        nextIndent = indent + 2
        leftNode = showTree l nextIndent
        rightNode = showTree r nextIndent
showTree BinEmpty _ = ""

maybeInsert :: (Ord k) => k -> v -> BinTree k v -> Maybe (BinTree k v)
maybeInsert k v BinEmpty = Just (BinNode k v BinEmpty BinEmpty)
maybeInsert k v (BinNode key val l r)
    | k == key = Nothing
    | k < key = if isSomething newL then Just lL else Nothing
    | otherwise = if isSomething newR then Just rR else Nothing
    where 
        newL = maybeInsert k v l
        newR = maybeInsert k v r
        isSomething Nothing = False
        isSomething(Just _) = True
        getSomething (Just x) = x
        lL = BinNode key val (getSomething newL) r
        rR = BinNode key val l (getSomething newR)


findBin :: Ord k => BinTree k a -> k -> [a]
findBin BinEmpty _ = []
findBin (BinNode k v l r) key
    | k == key = [v]
    | key < k = findBin l key
    | otherwise = findBin r key

insertL :: (Ord k) => BinTree k v -> (k, v) -> BinTree k v -- typová signatura odpovídá FOLDL
insertL BinEmpty (nKey, nValue) = BinNode nKey nValue BinEmpty BinEmpty
insertL (BinNode k v l r) new@(nKey, nValue)
    | k == nKey = BinNode k nValue l r
    | nKey < k = BinNode k v (insertL l new) r
    | otherwise = BinNode k v l (insertL r new)

insertTuplesL :: (Ord k) => BinTree k v -> [(k, v)] -> BinTree k v
insertTuplesL = foldl insertL

insertR :: (Ord k) => (k, v) -> BinTree k v -> BinTree k v -- typová signatura odpovídá FOLDR
insertR (nKey, nValue) BinEmpty = BinNode nKey nValue BinEmpty BinEmpty
insertR new@(nKey, nValue) (BinNode k v l r)
    | k == nKey = BinNode k nValue l r
    | nKey < k = BinNode k v (insertR new l) r
    | otherwise = BinNode k v l (insertR new r)

insertTuplesR :: (Ord k) => (BinTree k v) -> [(k, v)] -> BinTree k v
insertTuplesR = foldr insertR

insertTuples :: (Ord k) => BinTree k v -> [(k, v)] -> BinTree k v
insertTuples tree (x:xs) = insertTuples (insertL tree x) xs
insertTuples x [] = x

tupleList :: [(Integer, [Char])]
tupleList = [(64, "a"), (32, "b"), (128, "c"), (16, "d"), (96, "e")]

foldlTest :: Bool
foldlTest = foldl (&&) True ([True, True, False] ++ [True, True ..])

foldrTest :: Bool
foldrTest = foldr (&&) True ([True, True, False] ++ [True, True ..])

data MyMaybe a = MyNothing | MyJust a
instance (Show a) => Show (MyMaybe a) where
    show MyNothing = "MyNothing :("
    show (MyJust x)  = "MyJust :) " ++ show x

isMySomething MyNothing = False
isMySomething (MyJust _) = True

isMySome x = case x of
    MyNothing -> False
    (MyJust _) -> True

data OrdTest = OrdTest String (Int, Int)
instance Eq OrdTest where
    (==) (OrdTest _ (t1, t2)) (OrdTest _ (s1, s2)) = t1 == s1 && t2 == s2
instance Ord OrdTest where
    (<=) (OrdTest str1 _) (OrdTest str2 _) = length str1 <= length str2


tupTestTup (a, b) = a + b
tupTestParams a b = a + b

cat [] ys = ys
cat (x:xs) ys = x:(cat xs ys)
