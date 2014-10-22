H99P

11.

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
