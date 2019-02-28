module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
    fmap f fm = FunMonad $ f . (fun fm)

instance Applicative FunMonad where
    pure x = FunMonad (\_ -> x)
    (<*>) fm0 fm1 = FunMonad (\s -> (fun fm0) s (fun fm1 s))

instance Monad FunMonad where
    (>>=) fm0 f = FunMonad (\s -> fun (f ((fun fm0) s)) s)
