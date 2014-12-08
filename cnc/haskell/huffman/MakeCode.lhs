>	module MakeCode (codes, codeTable) --Tree -> Table

>	import Types
>	import Frequency ( frequency )
>	import MakeTree ( makeTree )
>	import CodeTable (codeTable )

Putting together frequency calculations and tree conversions

>	codes :: [Char] -> Tree
>	codes = makeTree . frequency

>	makeTree :: [(Char,Int)] -> Tree
>	makeTree = makeCodes . toTreeList

>	toTreeList :: [(Char,Int)] -> [Tree]


>	makeCodes :: [Tree] -> Tree	
>	makeCodes [t] = t
>	makeCodes ts = makeCodes (amalgamate ts)

>	toTreeList = map (uncurry Leaf)

>	amalgamate :: [Tree] -> [Tree]
>	amalgamate (t1:t2:ts) = insTree (pair t1 t2) ts

>	pair :: Tree -> Tree -> Tree
>	pair t1 t2 = Node (v1+v2) t1 t2
>		where
>			v1 = value t1
>			v2 = value t2

>	value :: Tree -> Int
>	value (Leaf _ n) = n
>	value (Node n _ _) = n

