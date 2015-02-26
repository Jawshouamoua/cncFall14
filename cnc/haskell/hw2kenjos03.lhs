>	import Data.Char

Lab 2 work
 
4.3

>	howManyEqual :: Integer -> Integer -> Integer -> Integer
>	howManyEqual a b c
>		| a == b && b == c		= 3
>		| a == b && b /= c		= 2
>		| a /= b && b == c		= 2
>		| a == c && b /= c		= 2
>		| otherwise				= 0 

4.9

>	cnt _ [] = 0
>	cnt n (x:xs)
>		| n == x	= 1 + cnt n xs
>		| otherwise	= 0 + cnt n xs

>	maxThreeOccurs :: Int -> Int -> Int -> (Int, Int) 
>	maxThreeOccurs a b c = (d, e)
>		where
>			d = maximum [a,b,c]
>			e = cnt d [a,b,c] 

4.10

Tim Sheard's Assignment 1

1)
3 :: Num a => a
even :: Integral a => a -> Bool
even 3 :: Bool

head :: [a] -> a
[1,2,3] :: Num t => [t]
head [1,2,3] :: Num a => a

When a polymorphic function is applied to a parameter, the type changes
to the actual type the fuction has been applied to.


2)

>	f1 :: (Float -> Float) -> Float
>	f1 a  = a 0.5

>	f2 :: Float -> (Float -> Float) 
>	f2 a b = a

>	f3 :: (Float -> Float) -> (Float -> Float)
>	f3 a b = a 0.5

3)

>	strlen [] 	= 0
>	strlen (x:xs) = 1 + strlen xs

4)

>	fact 0 = 1
>	fact n = n * fact (n-1)

5)

>	ncopies 0 _ = []
>	ncopies n m = [m] ++ ncopies (n-1) m

6)

>	power _ 1 = 1
>	power e x = e^^x

7)

>	convertStr [] = []
>	convertStr (x:xs) = [ord x] ++ convertStr xs

>	convertStr2 [] = []
>	convertStr2 (x:xs) = [x-48] ++ convertStr2 xs

>	createTuple [] = []
>	createTuple (x:xs) = [(x,floor((1 * 10^^n)))] ++ createTuple xs
>		where
>			n = strlen xs

>	multTuple [] = []
>	multTuple ((x,y):xys) = [x*y] ++ multTuple xys

>	str2Int [] = 0 
>	str2Int s = sum l
>		where
>			l = multTuple (createTuple (convertStr2 (convertStr s)))

CRFP

4.17

>	rangeProduct :: Int -> Int -> Int
>	rangeProduct m n
>		| m > n 	= 1
>		| otherwise = m * (rangeProduct (m+1) n)

>	t1 = rangeProduct 1 5
>	t2 = rangeProduct 1 2
>	t3 = rangeProduct 1 1

4.18

>	fac :: Int -> Int
>	fac n = rangeProduct 1 n

>	t4 = map fac [1..10]

4.19

>	mult :: Integer -> Integer -> Integer
>	mult x y 
>		| y == 0	= 0
>		|otherwise	= x + mult x (y-1)

>	t5 = [mult x y | x <- [1..5], y <- [1..5]]


4.20

>	sqRoot'' :: Integer -> Integer
>	sqRoot'' n = maximum $ sqRoot' 1 n

>	sqRoot' :: Integer -> Integer -> [Integer]
>	sqRoot' b n
>		| b*b > n	= [] 
>		| otherwise	= b : sqRoot' (b+1) n

>	t6 = map sqRoot'' [1..10]

4.21

>	max2n :: Integer -> Integer
>	max2n n = maximum $ max2n' n

>	max2n' :: Integer -> [Integer]
>	max2n' 0 = [fa 0]
>	max2n' n = (fa n) : max2n' (n-1)

>	fa :: Integer -> Integer
>	fa 0 = 0
>	fa 1 = 44
>	fa 2 = 17
>	fa _ = 0

>	t7 = map max2n [0..5]

4.31

>	divide m n
>		| m < n		= 0
>		| otherwise	= 1 + divide (m-n) n 

>	gcf :: Integer -> Integer -> Integer
>	gcf x y 
>		| x < 0 || y < 0	= 0
>		| y == 0 			= x
>		| otherwise			= gcf y (x `mod` y) 

>	t8 = [divide x y | x <- [1..10], y <- [1..5]]
>	t9 = [gcf x y | x <- [2,4..10], y <- [2,4..10]]

4.32
stuff 0 = 1
stuff x = 2 * stuff (x-1)

5.5

see below

5.7

>	data Shape = 	Circle Float |
>					Rectangle Float Float |
>					Triangle Float Float
>					deriving (Eq,Ord,Show) 

>	area :: Shape -> Float
>	area (Circle r)			= pi*r*r
>	area (Rectangle h w)	= h*w
>	area (Triangle h w) 	= (h*w)/2

>	perimeter :: Shape -> Float
>	perimeter (Circle r)		= 2 * pi * r
>	perimeter (Rectangle h w)	= (2 * h) + (2 * w) 
>	perimeter (Triangle h w)	= sqrt (h^^2 + w^^2)

>	shapes = [(Circle 4), (Circle 2), (Rectangle 2 2), (Rectangle 4 7),
>				(Triangle 4 4), (Triangle 6 2)]
>	t10 = map area shapes
>	t11 = map perimeter shapes

5.9

Eq class is for values that can be compared with each other. Eq is
dependent on (==) and (/=) being defined.

Ord class is for values that can be ordered. This builds off of Eq and
is dependent on (<=) being defined.

Show class is anything that can be transformed into a string. 
