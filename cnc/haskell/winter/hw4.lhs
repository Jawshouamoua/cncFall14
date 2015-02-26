 class Functor f => Pointed f where
	pure :: a -> f a


> class Functor f => Applicative f where
>	pure :: a -> f a
>	(<*>) :: f (a -> b) -> f a -> f b

> instance Applicative (Either a) where
> 	pure = Right
>	Left a <*> _ = Left a
>	Right f <*> r = fmap f r 

> instance Applicative [] where
>	pure x = [x]
>	gs <*> xs = [g x | g <- gs, x <- xs]

> instance Applicative ((->) r) where
>	pure x = (\_ -> x) 
>	f <*> g = \x -> f x (g x)

Proofs

instance Functor [] where
	fmap _ [] = []
	fmap g (x:xs) = g x : g x : fmap g xs

LHS
fmap id (x:xs) = (x:xs)

RHS
id x : id x : fmap id xs
x : x : xs or (x:x:xs)

these are not equivalent

instance Functor Maybe where
	fmap f (Just x) = Just (f x) 
	fmap f Nothing = Nothing

--fmap id = id

fmap id (Just x) 
= Just (id x)
= Just x 

Just (id x) 
= Just x

--fmap (g.h) = fmap g . fmap h

fmap (g.h) (Just x) 
= Just (g.h x)
= Just (g (h x))

fmap g . fmap h (Just x) 
= fmap g (fmap h (Just x))
= fmap g (Just (h x))
= Just (g (h x))

instance Pointed Maybe where
	pure = Just

--fmap g . pure = pure . g

fmap g . pure Nothing
= Nothing

pure . g Nothing
= Nothing

fmap g . pure (Just x) 
= fmap g (Just x)
= Just (g x)

pure . g (Just x) 
= pure (g (Just x))
= Just (g x)

instance Applicative Maybe where
	pure = Just
	Nothing <*> _ = Nothing
	(Just g) <*> x = fmap g x

--fmap g x = pure g <*> x

fmap g (Just x) 
= Just (g x)

pure g <*> (Just x)
= Just (g x)

instance Functor ((,)a) where
	fmap f (x,y) = (x, f y)

--fmap id = id

fmap id (x,y) 
= (x, id y) 
= (x , y)

--fmap (g.h) = fmap g . fmap h

fmap (g.h) (x,y) 
= (x, g (h y))

fmap g . fmap h (x,y)
= fmap g (x, h y)
= (x, g (h y))
