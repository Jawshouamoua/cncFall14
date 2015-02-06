> data Set a = Empty | Set [Integer]
> 	deriving Show

> member :: Integer -> Set a -> Bool
> member _ Empty = False
> member x (Set xs) = foldl (\acc y -> if y == x then True else acc) False xs
