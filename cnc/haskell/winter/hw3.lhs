> mapTuple :: (b -> c) -> (a,b) -> (a,c)
> mapTuple f (a,b) = (a, f b)

> newtype Pair a b = P (a,b)

> mapTuple' :: (b -> c) -> Pair a b -> Pair a c
> mapTuple' f (P (x,y)) = P (x, f y)

> instance Functor (Pair b) where
>	fmap = mapTuple'

instance Functor (a,b) where
	fmap = mapTuple'

> newtype Arrow r a =  Arrow (r -> a)

> mapArrow :: (b -> a) -> Arrow c b -> Arrow c a
> mapArrow f (Arrow g) = Arrow (\x -> f (g x)) 

> instance Functor (Arrow r) where
>	fmap = mapArrow

> newtype MyIO a = MyIO (IO a)

> mapIO :: (a -> b) -> MyIO a -> MyIO b
> mapIO f (MyIO m) = do
>	 x <- m 	
>	 return MyIO (f x) 
