
> import Prelude hiding (Maybe(..), Either(..))

> data Maybe a = Nothing | Just a
>	deriving Show

> data Either a b = Left a | Right b
>	deriving Show

> instance Monad Maybe where
>	return = Just
>	(Just x) >>= g = g x
>	Nothing >>= _ = Nothing
>	fail _ = Nothing

> t1 = Just 5 >>= \x -> Just (x + 5)
> t2 = Nothing >>= \x -> Just (x + 5)
> t3 = do
>	x <- Just 5
>	y <- Nothing
>	return (x + y)

> instance Monad (Either e) where
>	return x = Right x
>	(Left x) >>= g = (Left x)
>	(Right x) >>= g = g x

> t4 = Right 5 >>= \x -> Right (x + 5)
> t5 = Left 5 >>= \x -> Right (x+5)
> t6 = return 5 >>= \x -> Right (x+5)

> newtype Reader r a = Arrow (r -> a)
> runArrow (Arrow r) a = r a

> instance Monad (Reader r) where
>	return x = Arrow (\r -> x)
>	(Arrow g) >>= f = Arrow $ \r -> runArrow (f (g r)) r

> r1 :: Reader Int String
> r1 = let f x = show (x+1) in Arrow f

> t7 = putStrLn $ runArrow r1 1
> f1 = (\x -> head $ reverse $ take 5 $ iterate (+5) x)


 t8 = fmap f1 (Arrow (+5))
 t9 = Arrow (+5) >>= runArrow t8

> myap :: Monad m => m (a -> b) -> m a -> m b
> myap f x = do
>	f' <- f
>	x' <- x
>	return (f' x')

> myap' :: Monad m => m (a -> b) -> m a -> m b
> myap' f x = f >>= (\f' -> x >>= (\x' -> return (f' x')))

> x1 = Just (+5)
> x2 = Just (5)
> t10 = myap x1 x2
> t11 = myap' (return f1) x2

> mysequence :: Monad m => [m a] -> m [a] 
> mysequence ms = foldr k (return []) ms
>	where
>		k m m' = do
>			x <- m 
>			xs <- m'
>			return (x:xs)

> l1 = [Just 5, Just 2, Just 4, Just 8, Just 9]
> t12 = mysequence l1

> mymapM :: Monad m => (a -> m b) -> [a] -> m [b]
> mymapM f xs = mysequence $ fmap f xs

> t13 = mymapM (\x -> Just (x+5)) [1,2,3,4,5]

> fish :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
> fish g h = (\x -> g x >>= h)

> f3 = \x -> [x,-x]
> f4 = \x -> [x*3,x*2]
> t14 = fish f4 f3

> join' :: Monad m => (m (m a) -> m a) 
> join' x = x >>= id

> t15 = join' (Just (Just 5))
> t16 = join' (Just (Nothing))

> class Functor f => Applicative f where
>	pure :: a -> f a
>	(<*>) :: f (a -> b) -> f a -> f b	

> class Applicative m => JoinMonad m where
>	join :: m (m a) -> m a

> instance Functor Maybe where
>	fmap f Nothing = Nothing
>	fmap f (Just x) = Just (f x)

> instance Applicative Maybe where
>	pure = Just
>	Nothing <*> _ = Nothing
>	(Just g) <*> (Just x) = pure (g x)

> instance JoinMonad Maybe where
>	join (Just x) = x

> j1 = \x -> Just (x+5)
> j2 = join $ fmap j1 (Just 5) 
> j3 = Just 5 >>= j1

> instance Functor (Either e) where
>	fmap f (Left x) = Left x
>	fmap f (Right x) = Right (f x)

> instance Applicative (Either e) where
>	pure = Right
>	Left x <*> _ = Left x
>	Right x <*> r = fmap x r

> instance JoinMonad (Either e) where
>	join (Right x) = x
>	join (Left x) = Left x

> e1 = \x -> Right (x * 2 + 5)
> e2 = join $ fmap e1 (Right 1)


> instance Functor (Reader r) where
>	fmap f (Arrow g) = Arrow $ \x -> f (g x)

> instance Applicative (Reader r) where
>	pure g = Arrow $ \_ -> g
>	Arrow f <*> Arrow g = Arrow $ \x -> let 
>		x' = g x
>		f' = f x
>		in f' x'	

> instance JoinMonad (Reader r) where
>	join (Arrow g) = Arrow $ \x -> let
>		Arrow f = g x
>		in f x

> a1 = Arrow (\x -> (+2) x)  

 a2 = join $ fmap a1 (Arrow (+2) )

> sequence :: JoinMonad m => [m a] -> m [a] 
> sequence ms = foldr k [] ms where
>	k m ms' = 
