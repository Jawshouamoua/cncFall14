> module Queue(Queue(),isEmpty,enqueue,dequeue) where

> data Queue a = Empty | Item a (Queue a)
>	deriving Show

> isEmptyQueue :: Queue t -> Bool
> isEmptyQueue Empty = True
> isEmptyQueue (Item _ _) = False

> enqueue :: t -> Queue t -> Queue t
> enqueue x Empty = Item x Empty
> enqueue t (Item x xs) = Item x (enqueue t xs)

> dequeue :: Queue t -> Queue t -> (t, Queue t)
> dequeue Empty Empty = error "Can't dequeue empty queue"
> dequeue q (Item x Empty) = (x,q)
> dequeue q (Item x t) = dequeue (enqueue x q) t


