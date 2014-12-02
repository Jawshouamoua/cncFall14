>	app :: Num a => [a -> b] -> [a] -> [b]
>	app xs ys = zipWith ($) xs ys

>	testApp = take 4 $ app (map (*) [1,2..]) [1,3..]


>	chain :: (Integral a) => a -> [a] 
>	chain 1 = [1]
>	chain n
>		| even n	= n:chain (n `div` 2)
>		| odd n		= n:chain (n*3 + 1)

>	numLongChains :: Int
>	numLongChains = length (filter isLong (map chain [1..100]))
>		where isLong xs = length xs > 15
