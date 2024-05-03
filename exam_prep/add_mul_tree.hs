data Exp = Add Exp Exp | Mul Exp Exp | Value Int

pp (Value x) = show x
pp (Add left right) = pp left ++ '+':pp right
pp (Mul left@(Add _ _) right@(Add _ _)) = '(':(pp left) ++ (')':'*':'(':(pp right) ++ ")")
pp (Mul left@(Add _ _) right) = '(':(pp left) ++ (')':'*':(pp right))
pp (Mul left right@(Add _ _)) = (pp left) ++ ('*':'(':(pp right) ++ ")")
pp (Mul left right) = pp left ++ '*':pp right

expr1 = (Add (Mul (Value 1) (Value 2)) (Value 3))
expr2 = (Mul (Add (Value 1) (Value 2)) (Value 3))
expr3 = (Mul (Add (Value 1) (Value 2)) (Add (Value 3) (Value 4)))
expr4 = (Add (Add (Value 1) (Value 2)) (Add (Value 3) (Value 4)))
expr5 = (Mul (Mul (Value 1) (Value 2)) (Mul (Value 3) (Value 4)))
expr6 = (Mul (Mul (Value 1) (Value 2)) (Mul (Value 3) (Add (Value 4) (Value 5))))

ppOut = putStrLn . pp