>	import Data.Char

H99P
1.


>	myLast :: (Eq a) => [a] -> a
>	myLast (x:xs)
>		| xs == []		= x
>		| otherwise		= myLast xs

2.

>	myButList :: (Eq a) => [a] -> a
>	myButList l@(x:xs) 
>		| (length l) == 2 	= x
>		| otherwise			= myButList xs

3.

>	elementAt :: (Eq a) => [a] -> Int -> a
>	elementAt l n = l !! (n-1)

4.

>	myLength :: (Eq a) => [a] -> Int
>	myLength [] = 0
>	myLength (x:xs) = 1 + myLength xs

5.

>	myReverse :: (Eq a) => [a] -> [a]
>	myReverse [] = []
>	myReverse (x:xs) = myReverse xs ++ [x]

6.

>	isPalindrome :: (Eq a) => [a] -> Bool
>	isPalindrome l
>		| myReverse l == l	= True
>		| otherwise			= False

7.

>	data NestedList a = Elem a | List [NestedList a]

>	flatten :: NestedList a -> [a]
>	flatten (Elem a  )		= [a]
>	flatten (List (x:xs))	= flatten x ++ flatten (List xs)
>	flatten (List [])		= []

8.

>	compress :: (Eq a) => [a] -> [a]
>	compress (x:xs)
>		| xs == []		= [x]
>		| x == head xs	= compress xs
>		| otherwise		= x:compress xs

9.

>	pack :: (Eq a) => [a] -> [[a]]

>	pack [] = []
>	pack [x] = [[x]]
>	pack (x:xs) = if x `elem` (head (pack xs))
>					then (x:(head (pack xs))):(tail (pack xs))
>					else [x]:(pack xs)

10.

>	cnt [] = 0
>	cnt (x:xs) = 1 + cnt xs

	encode :: (Eq a) => [a] -> [(Int,a)]

	encode [] = []
	encode [x] = [(1,x)]
	encode l@(x:xs) = (cnt x', (head x')):encode xs'
			where
				l'@(x':xs') = pack l

>	encode xs = (enc . pack) xs
>		where enc = foldr (\x acc -> (length x, head x) : acc) []

2 CRFP
5.7

>	data Shape = 	Circle Float |
>					Rectangle Float Float |
>					Triangle Float Float
>					deriving (Eq, Ord, Show)

>	isRound :: Shape -> Bool
>	isRound (Circle _)		= True
>	isRound (Rectangle _ _)	= False
>	isRound (Triangle _ _) 	= False

>	area :: Shape -> Float
>	area (Circle r)			= pi*r*r
>	area (Rectangle b h)	= b*h
>	area (Triangle b h)		= (b*h)/2

>	perimeter :: Shape -> Float
>	perimeter (Circle r)		= 2 * pi * r
>	perimeter (Rectangle b h)	= (2*b) + (2*h)
>	perimeter (Triangle b h)	= (b*h)*2


5.15

The list from 0 to 1, incrementing 0.1 at a time. 
The list isn't 100% accurate because floating point
numbers tend to overshoot the limits.


5.16

[2,3] contains 2 elements in the list whereas [[2,3]] contains
2 element in the list. Its type is [[Integer]].

5.17

[2 .. 2] evaluates to 2 since it's the only item in the sequence.
[2, 7 .. 2] evaluates to 2 as well since there's no pattern to match.
[2, 2 .. 2] repeats 2s forever.


5.18

>	doubleAll :: [Integer] -> [Integer]
>	doubleAll l = [ 2*m | m<-l]

5.19

>	capitalizeLetters :: String -> String
>	capitalizeLetters l = [toUpper ch | ch<-l ]


5.20

>	divisors :: Integer -> [Integer]
>	divisors k 
>		| isPrime k	= [1,k]
>		| otherwise = [n | n<- [1 .. k], k `mod` n == 0] 


>	isPrime :: Integer -> Bool
>	isPrime k = null [ n | n<- [2 .. k], k `mod` n == 0]  


5.21

>	matches :: Integer -> [Integer] -> [Integer]
>	matches k l@(x:xs) = [n | n<-l, n == x]

>	elem' :: Integer -> [Integer] -> Bool
>	elem' x l 
>		| x == head (matches x l)	= True
>		| otherwise					= False	


5.22

>	onSeparateLines :: [String] -> String
>	onSeparateLines [] = []
>	onSeparateLines l@(x:xs) = x ++ "\n" ++ onSeparateLines xs

3 5.28


>	type Person = String
>	type Book 	= String

>	type Database = [ (Person, Book) ]

>	exampleBase :: Database
>	exampleBase = 
>			[ ("Alice", "Tintin"), ("Anna", "Little Women"),
>			("Alice", "Asterix"), ("Rory", "Tintin") ]

>	books :: Database -> Person -> [Book]
>	books db name = [n | (m,n) <- db, m == name ] 

>	borrowers :: Database -> Book -> [Person]
>	borrowers db book = [m | (m,n) <- db, n == book ]

>	borrowed :: Database -> Book -> Bool
>	borrowed [] book = False
>	borrowed ((m,n):xs) book
>		| book == n		= True
>		| otherwise		= False || borrowed xs book

>	numBorrowed :: Database -> Person -> Int
>	numBorrowed [] book = 0
>	numBorrowed ((m,n):xs) name
>		| name == m		= 1 + numBorrowed xs name
>		| otherwise		= 0 + numBorrowed xs name


4

>	exSet = [1,2,3,4,5,6,7,8,9,10]

	groupByN :: Int -> [a] -> [[a]]

>	groupByN [] n = []
>	groupByN l n = sublist:groupByN l' n
>		where
>			sublist = split l n
>			l' = drop n l

>	split _ 0 = []
>	split [] _ = []
>	split (x:xs) n = x:split xs (n-1)


	group :: Eq a => [a] -> [[a]]

>	group (x:y:xs)
>		| x==y		= [[]] ++ [x++y] ++ group (y:xs)
>		| otherwise	= [[]] ++ [x] ++ group (y:xs)
