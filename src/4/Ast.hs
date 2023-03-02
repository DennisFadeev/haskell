module Ast where
    data Exp t = Var t | Const Integer | Add (Exp t) (Exp t) deriving Show
    type Env t = t -> Integer

    eval :: Env t -> Exp t -> Integer
    eval env (Var v) = env v
    eval env (Const v) = v
    eval env (Add e1 e2) = (eval env e1) + (eval env e2)