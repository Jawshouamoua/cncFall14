import Data.Char
import Test.QuickCheck


>	size :: Integer
>	size = 12+13

>	square :: Integer -> Integer
>	square n = n*n

>	double :: Integer -> Integer
>	double n = 2*n

>	example :: Integer
>	example = double (size - square (2+2))	

*Main> square size
625
*Main> square

<interactive>:5:1:
    No instance for (Show (Integer -> Integer))
      arising from a use of `print'
    Possible fix:
      add an instance declaration for (Show (Integer -> Integer))
    In a stmt of an interactive GHCi command: print it
*Main> double (square 2)
8
*Main> square (double 2)
16
*Main> let d = double 2
*Main> square d
16
*Main> 23 - double (3+1)
15
*Main> 23 - double 3+1
18
*Main> it + 34
52
*Main> 13 `div` 5
2
*Main> 13 `mod` 5
3

3.9

>	threeDifferent :: Integer -> Integer -> Integer -> Bool
>	threeDifferent m n p = (m/=n) && (n/=p) && (m/=p)

*Main> threeDifferent 3 4 3
False

3.11a

threeEqual (2+3) 5 (11 `div` 2)
threeEqual 5 5 (11 `div` 2)
threeEqual 5 5 5 
(5 == 5) && (5 == 5)
True && True

3.12

>	myThreeDifferent :: Integer -> Integer -> Integer -> Bool
>	myThreeDifferent m n p = (m/=n) && (n /=p) && (m/=p) 
>	prop_myThreeDifferent :: Integer -> Integer -> Integer -> Bool 
>	prop_myThreeDifferent m n p = 
>		threeDifferent m n p == myThreeDifferent m n p

3.17

>	charToNum :: Char -> Int
>	charToNum a 
>		| (a >= '0') && (a <= '9')	= (fromEnum a) - 48
>		| otherwise					= 0


3.18

>	onThreeLines :: String -> String -> String -> IO ()
>	onThreeLines a b c = 
>		putStr (a ++ "\n" ++ b ++ "\n" ++ c ++ "\n")

3.20


>	averageThree :: Integer -> Integer -> Integer -> Float
>	averageThree a b c = fromIntegral (a + b + c) / 3.0 

>	howManyAbvAv :: Integer -> Integer -> Integer -> Integer
>	howManyAbvAv a b c
>		| a > d && b > d && c > d	= 3
>		| a > d && b > d && c <= d	= 2
>		| a > d && b <= d && c > d	= 2
>		| a <= d && b > d && c > d	= 2
>		| a <= d && b <= d && c > d	= 1
>		| a <= d && b > d && c <= d	= 1
>		| a > d && b <= d && c <= d	= 1
>		| otherwise					= 0
>			where
>				d = floor(averageThree a b c)

3.21


3.22

>	numberNDroots :: Float -> Float -> Float -> Integer
>	numberNDroots a b c
>		| d < 0		= 0
>		| d == 0	= 1
>		| d > 0		= 2
>			where
>				d = b^^2 - (4 * a * c)


