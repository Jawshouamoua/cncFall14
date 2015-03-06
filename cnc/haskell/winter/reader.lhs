

> newtype Reader e a = Reader { runReader :: (e -> a) }

> instance Monad (Reader e) where
>	return a = Reader $ \e -> a
>	(Reader r) >>= f = Reader $ \e -> runReader (f (r e)) e


