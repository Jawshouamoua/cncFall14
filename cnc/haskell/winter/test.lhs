> mapIO :: (a -> IO b) -> [a] -> [b] -> IO [b]
> mapIO f [] acc = do return acc
> mapIO f (x:xs) acc = do
> 	new <- f x
>	mapIO f xs (new:acc) 

> a1 = [Just 5, Just 2]

> a2 = Just 5
> a3 = Just 2
> a4 = (a2:a3:[]) 

> a5 = fmap ((:) a2) (Just [])

> a6 :: Maybe (Maybe a) -> Maybe [a]
> a6 (Just x) = x

