H99P


11


>	import Data.List

>	pack :: (Eq a) => [a] -> [[a]]
>	pack [] = []
>	pack [x] = [[x]]
>	pack (x:xs) = if x `elem` (head (pack xs))
>					then (x:(head (pack xs))):(tail (pack xs)) 
>					else [x]:(pack xs)

>	encode xs = (enc . pack) xs
>		where enc = foldr (\x acc -> (length x, head x) : acc) []

>	data ListItem a = Single a | Multiple Int a
>		deriving (Show)

>	encodeModified :: Eq a => [a] -> [ListItem a]
>	encodeModified = map encodeHelper . encode
>		where
>			encodeHelper (1,x) = Single x
>			encodeHelper (n,x) = Multiple n x


12

>	decodeModified :: Eq a => [ListItem a] -> [a]
>	decodeModified = concatMap decodeHelper
>		where
>			decodeHelper (Single x) = [x]
>			decodeHelper (Multiple n x) = replicate n x


13


14

>	dupli :: Eq a => [a] -> [a]

>	dupli [] = []
>	dupli (x:xs) = x:x:dupli xs 

>	repli xs n = concatMap (replicate n) xs


CRFP
7.8

>	elemNum :: Integer -> [Integer] -> Integer
>	elemNum _ [] = 0
>	elemNum n (x:xs) 
>		| n == x		= 1 + elemNum n xs
>		| otherwise		= 0 + elemNum n xs 

>	elemNum' :: Integer -> [Integer] -> Integer
>	elemNum' x xs = foldl (\acc y -> if x == y then 1 + acc else acc) 0 xs 
	

7.9

>	unique :: [Integer] -> [Integer]
>	unique xs = [x | x <- xs, elemNum' x xs == 1 ]	


10.9

	iter :: Integer -> (Integer -> Integer) -> Integer -> Integer

>	iter 0 _ x = x
>	iter n f x = f $ iter (n-1) f x

>	iter' n f x = unfoldr (\x -> Just (x, f x))    

11.17

>	addThree (x,y,z) = x+y+z

>	curry' :: ((a,b) -> c) -> (a -> b -> c) 
>	curry' f x y = f (x,y)

>	uncurry' :: (a -> b -> c) -> ((a,b) -> c) 
>	uncurry' f (x,y) = f x y 

>	curry3 :: ((a,b,c) -> d) -> (a -> b -> c -> d)
>	curry3 f x y z = f (x,y,z)

>	curry3' f x y z = curry' (curry' f x y) z   

>	uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d) 
>	uncurry3 f (x,y,z) = f x y z

11.18

	curryList :: ([a] -> d) -> (a -> [a] -> d) 

>	curryList f = \a -> \as -> f(a:as) 


	uncurryList :: (a -> [a] -> d) -> ([a] -> d) 

6.

>	composeList :: [b -> b] -> a -> b
>	composeList (x:xs) = x . composeList xs

>	composeList' xs = foldl (.) xs

>	groupByN n = takeWhile (not . null) . map fst . drop 1 . iterate (splitAt n . snd) . (\a -> ([],a))

	group : Eq a => [a] -> [[a]]



