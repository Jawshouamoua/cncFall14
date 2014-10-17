S -> 1A

>	s [] = False
>	s (x:xs) = v
>		where
>			v = a xs

A -> 0B | 11

>	a [] = False
>	a l@(x:y:xs)
>		| x == 1 && y == 1		= True
>		| x == 0				= v'
>			where
>				v' = b (y:xs)	

B -> 1A

>	b [] = False
>	b (x:xs) = v'
>		where
>			v' = a xs

