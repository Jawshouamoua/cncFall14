
> data Stack a = Empty | Item [a]
>	deriving Show

> isEmpty :: Stack t -> Bool
> isEmpty Empty = True
> isEmpty (Item _) = False

> push :: t -> Stack t -> Stack t 
> push x (Empty) = Item [x]
> push x (Item xs) = Item (x:xs)

> pop :: Stack t -> (t, Stack t) 
> pop Empty = error "can't pop from empty stack!"
> pop (Item (x:xs)) = (x, Item xs)

