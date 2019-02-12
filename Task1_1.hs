module Task1_1 where

import Todo(todo)

data Op =   Plus
            | Minus
            | Mult
            deriving(Show,Eq)

opfunTerm :: Op -> (Term -> Term -> Term)
opfunTerm Plus  = (|+|)
opfunTerm Minus = (|-|)
opfunTerm Mult  = (|*|)

opfunInt :: Op -> (Int -> Int -> Int)
opfunInt Plus  = (+)
opfunInt Minus = (-)
opfunInt Mult  = (*)

data Term = IntConstant{ intValue :: Int }  -- числовая константа
            | Variable{ varName :: String } -- переменная
            | BinaryTerm{ lhv :: Term,
                          rhv :: Term,
                          op  :: Op }       -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l r Plus
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l r Minus
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l r Mult

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
type VarName = String
type Replacement = Term
type Expression = Term
replaceVar :: VarName -> Replacement -> Expression -> Term
replaceVar _ _ (IntConstant i) = IntConstant i
replaceVar varName replacement (Variable v) =
    if v == varName then replacement else (Variable v)
replaceVar varName replacement (BinaryTerm l r op) = 
    BinaryTerm (rep l) (rep r) op
    where rep = replaceVar varName replacement

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm (IntConstant l) (IntConstant r) op) =
    IntConstant (opfunInt op l r)
evaluate (BinaryTerm (IntConstant 0) _ Mult) = IntConstant 0
evaluate (BinaryTerm _ (IntConstant 0) Mult) = IntConstant 0
evaluate (BinaryTerm (IntConstant 1) r Mult) = evaluate r
evaluate (BinaryTerm l (IntConstant 1) Mult) = evaluate l
evaluate (BinaryTerm (IntConstant 0) r _)    = evaluate r
evaluate (BinaryTerm l (IntConstant 0) _)    = evaluate l
evaluate (BinaryTerm l r op) = evaluate (opfunTerm op (evaluate l) (evaluate r))
evaluate expression = expression
