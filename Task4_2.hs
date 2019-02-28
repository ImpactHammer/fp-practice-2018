module Task4_2 where
import Control.Monad (ap, liftM)

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap f (FourOf x1 x2 x3 x4) = FourOf (f x1) (f x2) (f x3) (f x4)

instance Applicative FourOf where
    pure = return
    (<*>) (FourOf f1 f2 f3 f4) (FourOf x1 x2 x3 x4) =
        FourOf (f1 x1) (f2 x2) (f3 x3) (f4 x4)

instance Monad FourOf where
    return x = FourOf x x x x
    (>>=) (FourOf x1 x2 x3 x4) f = FourOf x1' x2' x3' x4' 
        where
        FourOf x1' _ _ _ = f x1
        FourOf _ x2' _ _ = f x2
        FourOf _ _ x3' _ = f x3
        FourOf _ _ _ x4' = f x4
