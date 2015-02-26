
> import Prelude hiding (Maybe(..), Either(..))

> data Maybe a = Nothing | Just a
>	deriving Show

> data Either a b = Left a | Right b
>	deriving Show

> instance Monad Maybe where
>	return = Just
>	(Just x) >>= g = g x
>	Nothing >>= _ = Nothing

