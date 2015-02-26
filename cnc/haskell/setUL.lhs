>	module SetUL (Set, null, member, empty, fromList, toList, insert,
>					delete)
>		where

>	import Prelude hiding (null)

>	newtype Set a = Set [a]

>	instance Eq a => Eq (Set a) where
>		(==) m1 m2 = all (`elem` (toList m2)) (toList m1)
>		(/=) m1 m2 = not $ all (`elem` (toList m2)) (toList m1)

>	instance Show a => Show (Set a) where
>		show (Set []) = "[]"
>		show (Set (x:xs)) = Prelude.show x ++ show (Set xs)


>	null :: Set a -> Bool
>	null (Set []) = True
>	null (Set s) = False

>	member :: Ord a => a -> Set a -> Bool
>	member n (Set []) = False
>	member n (Set (x:xs)) = x==n || member n (Set xs)

>	empty :: Set a
>	empty = Set []

>	fromList :: Ord a => [a] -> Set a 
>	fromList [] = Set []
>	fromList l = Set l

>	toList :: Set a -> [a]
>	toList (Set s) = s

>	insert :: Ord a => a -> Set a -> Set a 
>	insert n (Set xs) = Set (xs ++ n)

>	delete :: Ord a => a -> Set a -> Set a 
>	delete n (Set xs) = [x | x <- xs, x /= n] 
