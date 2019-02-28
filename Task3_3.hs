module Task3_3 where

newtype PSet a = PSet { contains :: (a -> Bool)}

newtype PSetAnd a = PSetAnd{ containsAnd :: (a -> Bool) }
newtype PSetOr  a = PSetOr{ containsOr :: (a -> Bool) }
newtype PSetXor a = PSetXor{ containsXor :: (a -> Bool) }
newtype PSetEq  a = PSetEq{ containsEq :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

{-| Monoid
    Можно использовать любую ассоциативную булеву функцию двух аргументов,
    имеющую нейтральный элемент. Например, and (True), or (False), xor (False), eq (True).
-}

instance Semigroup (PSetAnd a) where
    (<>) p0 p1 = PSetAnd $ \x -> (containsAnd p0) x  && (containsAnd p1) x
instance Semigroup (PSetOr a) where
    (<>) p0 p1 = PSetOr $ \x -> (containsOr p0) x  || (containsOr p1) x
instance Semigroup (PSetXor a) where
    (<>) p0 p1 = PSetXor $ \x -> (containsXor p0) x /= (containsXor p1) x
instance Semigroup (PSetEq a) where
    (<>) p0 p1 = PSetEq $ \x -> (containsEq p0) x  == (containsEq p1) x

instance Monoid (PSetAnd a) where
    mempty = PSetAnd (\_ -> True)
    mconcat [] = mempty
    mconcat (x:xs) = x <> (mconcat xs)
instance Monoid (PSetOr a) where
    mempty = PSetOr (\_ -> False)
    mconcat [] = mempty
    mconcat (x:xs) = x <> (mconcat xs)
instance Monoid (PSetXor a) where
    mempty = PSetXor (\_ -> False)
    mconcat [] = mempty
    mconcat (x:xs) = x <> (mconcat xs)
instance Monoid (PSetEq a) where
    mempty = PSetEq (\_ -> True)
    mconcat [] = mempty
    mconcat (x:xs) = x <> (mconcat xs)

{-| Functor
    Множество задано предикатом P0(X).
    Требуется построить предикат P1| P1(f(X)) === P0(X)
    P1(X) === P0(f[-1](X))

    Если f -- не биекция, то одному X может соответствовать бесконечно много
    значений {yi} == f[-1](X); в таком случае (P1(X) == True) <=> (существует yn| P0(yn) == True).
    Похоже на алгоритмически неразрешимую задачу. Предположим, что обратные функции 
    биективны и известны.
-}

data Bijective a b = Bijective {fun :: (a -> b),
                                inv :: (b -> a)}
class PredicateFunctor f where
    pmap :: Bijective a b -> f a -> f b
instance PredicateFunctor PSet where
    pmap f p = PSet $ (contains p) . (inv f)

mult2 = Bijective (*2) (/2)
nat = PSet (\x -> (x > 0 && x == fromInteger (round x)))
even = pmap mult2 nat
