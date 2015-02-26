>	import Data.Either
>	import Data.Char
>	import Test.QuickCheck

14.4

>	data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr
>				| Mul Expr Expr | Div Expr Expr

>	eval :: Expr -> Integer
>	eval (Lit n) = n
>	eval (Add e1 e2) = (eval e1) + (eval e2)
>	eval (Sub e1 e2) = (eval e1) - (eval e2)
>	eval (Mul e1 e2) = (eval e1) * (eval e2)
>	eval (Div e1 e2) = (eval e1) `div` (eval e2)

>	l1 = [(Add (Lit 5) (Lit 3)), (Sub (Lit 5) (Lit 2)), (Mul (Lit 2) (Lit 5)), (Div (Lit 6) (Lit 3))]
>	mapEval = map eval l1

>	show' :: Expr -> String
>	show' (Lit n) = show n 
>	show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
>	show' (Sub e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
>	show' (Mul e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"
>	show' (Div e1 e2) = "(" ++ show' e1 ++ "/" ++ show' e2 ++ ")"

>	mapShow = map show' l1

>	size :: Expr -> Integer
>	size (Lit n) = 0
>	size (Add e1 e2) = 1 + (size e1) + (size e2)
>	size (Sub e1 e2) = 1 + (size e1) + (size e2)
>	size (Mul e1 e2) = 1 + (size e1) + (size e2)
>	size (Div e1 e2) = 1 + (size e1) + (size e2)

>	l2 = (Add (Add (Add (Lit 5) (Lit 2)) (Lit 3)) (Lit 2))
>	size1 = size l2
>	mapSize' = map size l1

14.5

14.6

14.8

14.10

>	data NTree = NilT | Node Integer NTree NTree

>	sumTree,depth :: NTree -> Integer
>	sumTree NilT = 0
>	sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

>	depth NilT = 0
>	depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

>	occurs :: NTree -> Integer -> Integer
>	occurs NilT _ = 0 
>	occurs (Node n t1 t2) p
>		| n==p		= 1 + occurs t1 p + occurs t2 p
>		| otherwise = occurs t1 p + occurs t2 p

>	element :: NTree -> Integer -> Bool
>	element NilT _ = False
>	element (Node n t1 t2) p
>		| n == p		= True
>		| otherwise		= (element t1 p) || (element t2 p)

14.13

>	collapse,sort' :: NTree -> [Integer]
>	collapse NilT = []
>	collapse (Node n t1 t2) = [n] ++ (collapse t1) ++ (collapse t2)

>	sort' NilT = []
>	sort' tree = sort $ collapse tree

>	sort :: (Ord a) => [a] -> [a]
>	sort [] = []
>	sort (x:xs) = (sort lesser) ++ [x] ++ (sort greater)
>		where
>			lesser = filter (< x) xs
>			greater = filter (>= x) xs


14.15


14.19

	data Either a b = Left a | Right b
			deriving (Eq, Ord, Read, Show)

>	isLeft :: Either a b -> Bool
>	isLeft (Left _) = True
>	isLeft (Right _) = False

	fun :: Either a b -> Bool
	fun (Left x) = 

	either :: (a -> c) -> (b -> c) -> Either a b -> c
	either f g (Left x) = f x
	either f g (Right y) = g y

>	applyLeft :: (a -> c) -> Either a b -> c
>	applyLeft f (Left x) = f x
>	applyLeft f (Right x) = error "applyLeft applied to right"



4.20

	join :: (a -> c) -> (b -> d) -> Either a b -> Either c d
	join f g (Left x) = applyLeft f x

	join f g (Right y) = 

14.23


14.25


14.26


14.27


5.

>	data Bit = Zero | One
>		deriving (Show) 
>	instance Eq Bit where
>		Zero == Zero = True 
>		One == One = True
>		Zero == One = False
>		One == Zero = False

>	bin2int :: [Bit] -> Int
>	bin2int [] = 0
>	bin2int (Zero:xs) = 0 + bin2int xs
>	bin2int (One:xs) = (2 ^ length xs) + bin2int xs

>	int2bin :: Int -> [Bit]
>	int2bin 0 = []
>	int2bin n 
>		| n `mod` 2 == 1	= One : int2bin (n `div` 2)
>		| n `mod` 2 == 0 	= Zero : int2bin (n `div` 2)

>	int2bin' :: Int -> [Bit]
>	int2bin' b = reverse $ int2bin b

>	transmit :: String -> String
>	transmit = decode . channel . encode

>	channel :: [Bit] -> [Bit]
>	channel = id

>	encode :: String -> [Bit]
>	encode = convert' . convert 

>	convert' :: [Int] -> [Bit]
>	convert' [] = []
>	convert' (x:xs) = (convert'' $ int2bin' x) ++ convert' xs

>	convert'' :: [Bit] -> [Bit]
>	convert'' b 
>		| length b >= 8	= b
>		| length b < 8  = convert'' ([Zero] ++ b)

>	convert :: String -> [Int]
>	convert s = map ord s

>	decode :: [Bit] -> String
>	decode = deconvert . deconvert'

>	deconvert' :: [Bit] -> [Int]
>	deconvert' [] = []
>	deconvert' b = (bin2int $ take 8 b) : deconvert' (drop 8 b)

>	deconvert :: [Int] -> String
>	deconvert s = map chr s


11.

>	bin2int' :: [Bit] -> Int
>	bin2int' = foldl (\x y -> if y==One then x*2 + 1 else x*2) 0

12. 

>	unfold' f g h = takeWhile f . map g . iterate h 

>	unfold p h t x
>		| p x		= []
>		| otherwise = h x : unfold p h t (t x)

>	prop_unfold f g h x = unfold f g h x == unfold' f g h x

	main = quickCheck prop_unfold


	int2bin :: Int -> [Bit]
	int2bin 1 = []
	int2bin n 
		| n `mod` 2 == 1	= One : int2bin (n `div` 2)
		| n `mod` 2 == 0 	= Zero : int2bin (n `div` 2)

>	unfoldBit p h t x
>		| p x		= [Zero]
>		| otherwise = h x : unfoldBit p h t (t x)

>	int2binf :: Int -> [Bit]
>	int2binf = map helper'. reverse . takeWhile (>0) . iterate div2   

>	helper' :: Int -> Bit
>	helper' n = helper $ mod n 2

>	div2 :: Int -> Int
>	div2 n = n `div` 2

>	helper :: Int -> Bit
>	helper n 
>		| n == 0		= Zero
>		| n == 1		= One

