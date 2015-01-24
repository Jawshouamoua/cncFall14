> import Queue(isEmpty,enqueue,dequeue)

> isEmpty :: Queue t -> Bool
> isEmpty Empty = True
> isEmpty (Item _ _) = False
