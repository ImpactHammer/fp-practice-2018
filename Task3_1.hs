module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize = toEnum . fromEnum

toWpn :: Int -> WeirdPeanoNumber
toWpn = toEnum

instance Enum WeirdPeanoNumber where
    succ w = Succ w
    pred w = Pred w
    fromEnum w = case w of
        Zero -> 0
        Succ w0 -> fromEnum w0 + 1
        Pred w0 -> fromEnum w0 - 1
    toEnum i
        | i == 0 = Zero
        | i < 0 = Pred $ toEnum (i + 1)
        | otherwise = Succ $ toEnum (i - 1)

instance Eq WeirdPeanoNumber where
    (==) w0 w1 = case (wn0, wn1) of 
        (Zero, Zero) -> True
        (Zero, Succ _) -> False
        (Zero, Pred _) -> False
        (Succ _, Zero) -> False
        (Pred _, Zero) -> False
        (Succ _, Pred _) -> False
        (Pred _, Succ _) -> False
        (Succ wn0, Succ wn1) -> wn0 == wn1
        (Pred wn0, Pred wn1) -> wn0 == wn1
        where 
        wn0 = normalize w0
        wn1 = normalize w1
    (/=) w0 w1 = not (w0 == w1)

instance Integral WeirdPeanoNumber where
    toInteger = toInteger . fromEnum
    quotRem w0 w1 = case wn0 of
        Zero -> (Zero, Zero)
        cur@(Succ _) -> if dif < Zero 
            then (Zero, cur)
            else (ctr $ fst (quotRem dif wn1),
                         snd (quotRem dif wn1))
            where 
            dif = (cur - abs wn1)
            ctr = if (signum w1 == 1) then Succ else Pred
        cur@(Pred _) -> if dif > Zero 
            then (Zero, cur)
            else (ctr $ fst (quotRem dif wn1),
                         snd (quotRem dif wn1))
            where 
            dif = (cur + abs wn1)
            ctr = if (signum w1 == 1) then Pred else Succ
        where
        wn0 = normalize w0
        wn1 = normalize w1

instance Num WeirdPeanoNumber where
    (+) w0 w1 = case wn0 of
        Zero -> wn1
        Succ s -> Succ (s + wn1)
        Pred p -> Pred (p + wn1)
        where
        wn0 = normalize w0
        wn1 = normalize w1
    (-) w0 w1 = wn0 + (negate wn1)
            where
            wn0 = normalize w0
            wn1 = normalize w1
    (*) w0 w1 = case wn0 of
        Zero -> Zero
        Succ s -> (s * wn1) + wn1
        Pred p -> (p * wn1) - wn1
        where
        wn0 = normalize w0
        wn1 = normalize w1
    negate w = case wn of
        Zero -> Zero
        Succ s -> Pred $ negate s
        Pred s -> Succ $ negate s
        where
        wn = normalize w
    abs w = case wn of
        Pred s -> Succ $ abs s 
        _ -> wn
        where
        wn = normalize w
    signum w = case wn of
        Zero -> 0
        Succ _ -> 1
        Pred _ -> (-1)
        where
        wn = normalize w
    fromInteger i
        | i == 0 = Zero
        | i < 0 = Pred $ fromInteger (i + 1)
        | otherwise = Succ $ fromInteger (i - 1)

instance Ord WeirdPeanoNumber where
    compare w0 w1 = case (wn0, wn1) of
        (Zero   , Succ _ ) -> LT
        (Pred _ , Succ _ ) -> LT
        (Succ s0, Succ s1) -> compare s0 s1
        (Zero   , Pred _ ) -> GT
        (Succ _ , Pred _ ) -> GT
        (Pred p0, Pred p1) -> compare p0 p1
        (Pred _ , Zero   ) -> LT
        (Succ _ , Zero   ) -> GT
        (Zero   , Zero   ) -> EQ
        where
        wn0 = normalize w0
        wn1 = normalize w1
    (<)  w0 w1 = (compare w0 w1) == LT
    (<=) w0 w1 = (w0 < w1) || (w0 == w1)
    (>)  w0 w1 = not (w0 <= w1)
    (>=) w0 w1 = not (w0 < w1)

instance Real WeirdPeanoNumber where
    toRational = toRational . fromEnum

instance Show WeirdPeanoNumber where
    show w = case w of
        Zero -> "Zero"
        Succ s -> "Succ " ++ (show s)
        Pred p -> "Pred " ++ (show p)
