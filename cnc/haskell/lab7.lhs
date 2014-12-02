> module SetUL (Set, null, member, empty, fromList, toList, insert, delete)
>	where

>	import Prelude hiding (null)

>	newtype Set a = Set [a]

