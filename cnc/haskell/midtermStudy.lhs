

>	sul ys = all (`elem` ys)


>	twasf p = foldl clip [] where
>		clip x xs 
>			| p x 		= x : xs
>			|otherwise	= []

>	whatFun4 p = (\x xs -> if p x then x:xs else []) []
