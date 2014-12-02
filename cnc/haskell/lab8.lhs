>	module SetUL (Set, null, member, empty, fromList, toList, insert,
>					delete)
>		where

>	import Prelude hiding (null)

>	newtype Set a = Set [a]

>	instance Ord Set where



>	null :: Set a -> Bool
>	null (Set []) = True
>	null (Set s) = False

>	member :: Ord a => a -> Set a -> Bool
>	member n (Set []) = False
>	member n (Set (x:xs)) = x==n || member n (Set xs)

>	empty :: Set a
>	empty = Set []

>	fromList :: Ord a => [a] -> Set a 
>	fromList l = Set l

>	toList :: Ord a => Set a -> [a]
>	toList (Set s) = s

>	insert :: Ord a => a -> Set a -> Set a 
>	insert n (Set xs) = insert n (Set xs)

>	delete :: Ord a => a -> Set a -> Set a 
>	delete n (Set xs) = delete n (Set xs) 
