
data Expr = Add Expr Expr | Const Int | Var String

instance Show Expr where
    show t = showIndented t 0

showIndented (Add l r) i = take i [' ', ' ' ..] ++ "Add\n" ++ showIndented l (i + 2) ++ showIndented r (i + 2)
showIndented (Const x) i = take i [' ', ' ' ..] ++ show x ++ "\n"
showIndented (Var x) i =   take i [' ', ' ' ..] ++ show x ++ "\n"

tree = Add (Add (Const 2) (Add (Add (Const 4) (Const 5)) (Const 3))) (Add (Add (Var "lol") (Add (Var ":D") (Const 6))) (Add (Const 10) (Const 2)))

simplify (Add (Const x) (Const y)) = Const (x+y)
simplify (Add x y)
    | isConst left && isConst right = Const (getValue left + getValue right)
    | otherwise = Add left right
    where
        left = simplify x
        right = simplify y
        isConst (Const _) = True
        isConst _ = False
        getValue (Const x) = x -- !!! necessary to extract the value out of Const !
simplify x = x
