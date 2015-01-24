
> data Queue a = Empty | Item a (Queue a) 
>	deriving Show

> isEmptyQueue :: Queue t -> Bool
> isEmptyQueue Empty = True
> isEmptyQueue (Item _  _) = False

> enqueue :: t -> Queue t -> Queue t
> enqueue x Empty = Item x Empty
> enqueue t (Item x xs) = Item x (enqueue t xs)

> dequeue' :: Queue t -> Queue t -> (t, Queue t)
> dequeue' Empty Empty= error "Can't dequeue empty Queue"
> dequeue' q (Item x Empty) = (x, q)
> dequeue' q (Item x t) = dequeue' (enqueue x q) t


> data Queue2 a = Queue2 [a] [a] 

> empty (Queue2 [] []) = True
> empty _ = False

> enq :: t -> Queue2 t -> Queue2 t
> enq y (Queue2 xs ys) = Queue2 xs (y:ys)

> deq :: Queue2 t -> (t, Queue2 t)
> deq (Queue2 [] []) = error "Can't deq from empty queue"
> deq (Queue2 (x:xs) ys) = (x, Queue2 xs ys) 
