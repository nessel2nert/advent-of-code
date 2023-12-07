newtype State s a = ST { runST :: s -> (a, s) }

instance Functor (State s) where
    fmap f (ST s) = ST s' where
        s' = let
            trans1 (e1, e2) = (f e1, e2) 
            in trans1 . s

instance Applicative (State s) where
    pure x = ST (x,)
    (ST f) <*> (ST s) = ST s' where
        s' input = let
            (f', next) = f input
            (x, next') = s next
            in (f' x, next')

instance Monad (State s) where
    return = pure
    (ST s) >>= f = ST s' where
        s' input = let
            (x, next) = s input
            in runST (f x) next

get :: State s s
get = ST $ \input -> (input, input)

put :: s -> State s ()
put x = ST $ const ((), x)

modify :: (t -> t) -> State t ()
modify f = do
    s <- get
    put $ f s