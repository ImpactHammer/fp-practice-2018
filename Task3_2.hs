module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons xs x) = x:(rlistToList xs)

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList (x:xs) = RCons (listToRList xs) x

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Eq a) => Eq (ReverseList a) where
    (==) rl0 rl1 = case (rl0, rl1) of
        (RNil, RNil) -> True
        (RNil, _) -> False
        (_, RNil) -> False
        ((RCons xs0 x0), (RCons xs1 x1))
            | x0 /= x1 -> False
            | otherwise -> xs0 == xs1

instance (Ord a) => Ord (ReverseList a) where
    compare rl0 rl1 = case (rl0, rl1) of
        (RNil, RNil) -> EQ
        (RNil, _) -> LT
        (_, RNil) -> GT
        ((RCons xs0 x0), (RCons xs1 x1))
            | x0 == x1 -> compare xs0 xs1
            | x0 < x1 -> LT
            | otherwise -> GT

instance (Show a) => Show (ReverseList a) where
    show rl = "[" ++ (show' rl) ++ "]" where
        show' RNil = ""
        show' (RCons RNil x) = (show x)
        show' (RCons xs x) = (show x) ++ "," ++ (show' xs)

instance Semigroup (ReverseList a) where
    (<>) rl0 rl1 = case rl0 of
        RNil -> rl1
        (RCons xs0 x0) -> RCons (xs0 <> rl1) x0

instance Monoid (ReverseList a) where
    mempty = RNil
    mconcat [] = RNil
    mconcat (x:xs) = x <> (mconcat xs)

instance Functor ReverseList where
    fmap f rl = case (f, rl) of
        (_, RNil) -> RNil
        (f, RCons xs x) -> RCons (fmap f xs) (f x)
