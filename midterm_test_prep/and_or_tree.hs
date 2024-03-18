
data Expr = And Expr Expr | Or Expr Expr | Const Bool | Var String

instance Show Expr where
    show e = tail (showIndent e 0)
showIndent (And l r) i = "\n" ++ (take i [' ', ' '..]) ++ "And:" ++ (showIndent l (i+2)) ++ (showIndent r (i+2))
showIndent (Or l r) i = "\n" ++ (take i [' ', ' '..]) ++ "Or:" ++ (showIndent l (i+2)) ++ (showIndent r (i+2))
showIndent (Const x) i = " " ++ show x ++ " "
showIndent (Var x) i = " " ++ x ++ " "

tree1 = And (Or (And (Const True) (Var "X")) (Var "A")) (Or (And (Const True) (Var "B")) (Const False))
tree2 = And (Or (And (Const True) (Var "X")) (Const True)) (Or (And (Const True) (Var "B")) (Const False))
tree3 = And (Or (And (Const True) (Var "X")) (Const True)) (Or (And (Const True) (Var "B")) (And (Or (Const True) (Var "Y")) (Const True)))
tree4 = And (Or (And (Const True) (Var "X")) (Var "A")) (Or (And (Const False) (Var "B")) (Const False))

eval :: Expr -> Expr
eval (And (Const False) _) = Const False
eval (And _ (Const False)) = Const False
eval (And (Const True) r) = eval r
eval (And l (Const True)) = eval l
eval (Or (Const True) _) = Const True
eval (Or _ (Const True)) = Const True
eval (Or (Const False) r) = eval r
eval (Or l (Const False)) = eval l
eval (And l r)
    | lEC && not lC = Const False
    | rEC && not rC = Const False
    | lEC && rEC && lC && rC = Const True
    | lEC && lC = rE
    | rEC && rC = lE
    | otherwise = And lE rE
    where
        lE = eval l
        rE = eval r
        isConst (Const _) = True
        isConst _ = False
        getConst (Const x) = x
        lEC = isConst lE
        rEC = isConst rE
        lC = getConst lE
        rC = getConst rE
eval (Or l r)
    | lEC && lC = Const True
    | rEC && rC = Const True
    | lEC && rEC && not lC && not rC = Const False
    | lEC && not lC = rE
    | rEC && not rC = lE
    | otherwise = Or lE rE
    where
        lE = eval l
        rE = eval r
        isConst (Const _) = True
        isConst _ = False
        getConst (Const x) = x
        lEC = isConst lE
        rEC = isConst rE
        lC = getConst lE
        rC = getConst rE
eval x = x
