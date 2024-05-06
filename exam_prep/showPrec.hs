
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

instance Show Expr where
    showsPrec _ (Val n) = showString $ show n
    showsPrec p (Add e1 e2) = showParen (p > 6) $ showsPrec 6 e1 . showString " + " . showsPrec 7 e2
    showsPrec p (Mul e1 e2) = showParen (p > 7) $ showsPrec 7 e1 . showString " * " . showsPrec 8 e2

expr = (Mul (Add (Val 1) (Val 3)) (Val 6))