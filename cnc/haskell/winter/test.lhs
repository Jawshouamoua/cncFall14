> mapIO :: (a -> IO b) -> [a] -> [b] -> IO [b]
> mapIO f [] acc = do return acc
> mapIO f (x:xs) acc = do
> 	new <- f x
>	mapIO f xs (new:acc) 


