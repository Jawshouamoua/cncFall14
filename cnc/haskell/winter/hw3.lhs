> mapTuple :: (b -> c) -> (a,b) -> (a,c)
> mapTuple f (a,b) = (a, f b)

> newtype Pair a b = P (a,b)

> mapTuple' :: (b -> c) -> Pair a b -> Pair a c
> mapTuple' f (P (x,y)) = P (x, f y)

> instance Functor (Pair b) where
>	fmap = mapTuple'

instance Functor (a,b) where
	fmap = mapTuple'

> newtype Arrow a = 

> mapArrow :: (b -> a) -> (c -> b) -> c -> a
> mapArrow f g = (\x -> f (g x)) 

 instance Functor ((->) r) where
	fmap = mapArrow


