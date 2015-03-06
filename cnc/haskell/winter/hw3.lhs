> import Prelude hiding (Maybe(..), Either(..))


> mapTuple :: (b -> c) -> (a,b) -> (a,c)
> mapTuple f (a,b) = (a, f b)

> newtype Pair a b = P (a,b)

> mapTuple' :: (b -> c) -> Pair a b -> Pair a c
> mapTuple' f (P (x,y)) = P (x, f y)

> instance Functor (Pair b) where
>	fmap = mapTuple'

> t1 = fmap (+5) ("hello", 5)
> t2 = fmap f1 ("hello", 5)
> f1 = (\x -> head $ reverse $ take 5 $ iterate (+5) x) 

instance Functor (a,b) where
	fmap = mapTuple'

> newtype Arrow r a =  Arrow (r -> a)
> runArrow (Arrow f) x = f x

> mapArrow :: (b -> a) -> Arrow c b -> Arrow c a
> mapArrow f (Arrow g) = Arrow $ \x -> f (g x) 

> instance Functor (Arrow r) where
>	fmap = mapArrow

> t3 = fmap f1 (Arrow (+5)) 


> newtype MyIO a = MyIO (IO a)

> mapIO :: (a -> b) -> MyIO a -> MyIO b
> mapIO f (MyIO m) = MyIO $ fmap f m



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

