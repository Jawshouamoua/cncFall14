> import Data.Char

8.10


> getPali :: IO()
> getPali =	do 
>		putStrLn "Input string"
>		line <- getLine
>		putStrLn (reverse line)

8.11

> addNum :: IO()
> addNum = do
>		putStrLn "Input two numbers, output sum"
>		line <- getLine
>		line1 <- getLine
>		putStr "Sum:"
>		putStrLn show $ (read line) + (read line1)

8.12

> putNtimes :: Integer -> String -> IO() 
> putNtimes n str =
>		if n <= 0
>		then return ()
>		else do 
>			putStrLn str
>			putNtimes (n-1) str


8.13

> varSum :: IO()
> varSum = do
>		putStrLn "input positive integer"
>		n <- getLine
>		if (read n) <= 0 
>		then return()
>		else varSum' 0 (read n)

> varSum' :: Integer -> Integer -> IO() 
> varSum' a n = do
>		if n <= 0 
>		then do
>			putStrLn (show a)
>		else do
>			num <- getLine
>			varSum' ((read num) + a) (n-1)

8.14

> wc :: Integer -> Integer -> Int -> IO()
> wc l w c = do 
>		putStrLn ("input a string") 
>		line <- getLine
>		if line == ""
>			then do
>				putStrLn ("Lines read:" ++ (show l))
>				putStrLn ("words read:" ++ (show w))		
>				putStrLn ("chars read:" ++ (show c)) 
>		else do
>			putStrLn line
>			wc (l+1) ((wordCnt line)+w) (c + (length line))

> wordCnt :: String -> Integer
> wordCnt [] = 0
> wordCnt (x:xs)
>	|x == ' '	= 1+wordCnt xs  
>	|otherwise  = 0+wordCnt xs


8.15

> isPali :: IO()
> isPali = do
>		putStrLn ("input string to check if palindrome")
>		line <- getLine
>		if (reverse $ toLower' line) == (toLower' line)
>			then putStrLn ("This is a palindrome, yooooo")
>		else 
>			putStrLn ("Not a palindrome")

> toLower' :: [Char] -> [Char]
> toLower' s = rmChar $ map toLower s

> chars :: [Char]
> chars = ['a'..'z'] 

> rmChar :: [Char] -> [Char]
> rmChar s = [x | x <- s, elem x chars] 

8.16

> paliChecker :: IO()
> paliChecker = do
>		putStrLn ("input string")
>		line <- getLine
>		if line == ""
>			then do
>			putStrLn ("need a string bro")
>			return()
>		else do
>			if (reverse $ toLower' line) == (toLower' line)
>				then putStrLn ("this is a palindrome dude")
>			else
>				putStrLn ("not a palindrome yooo")
>		paliChecker


8.17

> sumZero :: IO()
> sumZero = readSum 0

> readSum :: Integer -> IO()
> readSum x = do
>		putStrLn ("input number")
>		line <- getLine
>		if (read line) == 0 
>		then do
>			putStrLn (show x)
>		else
>			readSum (x+(read line))


8.18

> mkList :: [Integer] -> IO()
> mkList s = do
>		putStrLn ("input number, duuude")
>		line <- getLine
>		if (read line) == 0
>		then do
>			let s' = insertion_sort (<=) s
>			putStr ("[")
>			let prntLst l@(x:xs) = do
>				if xs == []
>				then do 	
>					putStrLn ((show x) ++ "]")
>				else do
>					putStr ((show x) ++ ",")
>					prntLst xs	
>			prntLst s'
>		else do
>			mkList ((read line):s)

> insertion_sort :: (a -> a -> Bool) -> [a] -> [a]
> insertion_sort pred []	= []
> insertion_sort pred (x:xs) = insert pred x (insertion_sort pred xs)

> insert :: (a -> a -> Bool) -> a -> [a] -> [a] 
> insert pred x [] = [x] 
> insert pred x (y:ys)
> 		| pred x y = (x:y:ys)
> 		| otherwise = y:(insert pred x ys)


8.19

whileCopy reads a string and outputs the string until and empty string
is read. 
