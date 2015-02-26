> import Prelude hiding (Maybe(..), Either(..))


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
>	 x <- f m 	
>	 return MyIO x


> class Functor f => Pointed f where 
>	pure :: a -> f a

> instance Pointed ((->) r) where
>	pure x = (\_ -> x) 

> data Maybe a = Nothing | Just a
>	deriving Show

> mapMaybe :: (a -> b) -> Maybe a -> Maybe b
> mapMaybe _ Nothing = Nothing
> mapMaybe f (Just a) = Just (f a)

> instance Functor Maybe where
>	fmap = mapMaybe

> instance Pointed Maybe where
>	pure = Just

The purpose of the pointed class is to lift a value's context to that
of the container without actually changing the container or having
to wrap a value back up into the correct context. Pointed provides a
basis for Applicative, although Applicative was created first. 

Tuples can't be made an instance of Pointed because trying to lift a 
value to the context of a tuple means generating the value e from
"pure :: a -> (e, a)" without defining e. Tuples have extra baggage 
that disallows the pureness of it. 

