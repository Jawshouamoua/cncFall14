>	module MakeCode (codes, codeTable) --Tree -> Table
>		where
>	import Types
>	import Frequency ( frequency )
>	import MakeTree ( makeTree )
>	import CodeTable (codeTable )

Putting together frequency calculations and tree conversions

>	codes :: [Char] -> Tree
>	codes = makeTree . frequency



