A proof of the Functor Laws for Binary Trees in Haskell

NOTE: THE PROOF IS INCOMPLETE. 
You'll need to fill in the missing parts

Here's a proof for binary trees that gives a detailed form for the
proof. The proof for GTrees differs from this proof in the need to handle
the list map of a function over the list of sub-GTrees of a node.


> data Tree a = NilT | Node a (Tree a) (Tree a) deriving Show
> instance Functor Tree where
>   fmap f NilT         = NilT
>   fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
>
> -- A simple test 
>

Prove the Functor Laws for the Tree functor (tf.1 and tf.2)
fmap id    = id                   (tf.1)
fmap (g.h) = fmap g . fmap h      (tf.2)

The proof is by induction because the Tree data type is not finite.

------------------------------
(a) Claim: (tf.1) fmap id = id
------------------------------
The proof is by structural induction over the height of the tree.
We need to show: fmap id t = id t for all binary trees t

BASE case: the tree of form NilT
Show: fmap id NilT = id NilT

(i) Left side of equation
fmap id NilT = NilT

(ii) Right side of equation
id NilT = NilT

So both sides of the equation are equal for the NilT tree.


INDUCTION STEP case: the tree of form (Node x l r)
Show: fmap id (Node x l r) = id (Node x l r)        for all subtrees l, r

(i) Left side of equation
fmap id (Node x l r) 
  = Node (id x) (fmap id l)  (fmap id r)    -- definition of fmap
  = Node (id x) l r                         -- structural induction hypothesis
  = Node x l r                              -- definition of id

(ii) Left side of equation
id (Node x l r)
  = Node x l r                              -- defintion of id

So again, both sides of the equation are equal for the tree of form (Node x l r)

By the principle of structural induction over binary trees
The claim: (tf.1) fmap id = id
  is True
QED proof of (tf.1)

----------------------------------------------
(b) Claim: (tf.2) fmap (g.h) = fmap g . fmap h
----------------------------------------------
Again, the proof is by structural induction over the height of the tree.
We need to show: fmap (g.h) t = (fmap g t . fmap h t) for all binary trees t

BASE case: the tree of form NilT
Show: fmap (g.h) NilT = fmap g NilT . fmap h NilT

(i) Left side of equation
fmap (g.h) NilT = NilT

(ii) Right side of equation
fmap g . fmap h NilT = NilT

So both sides of the equation are equal for the NilT tree.


INDUCTION STEP case: the tree of form (Node x l r)
Show: fmap (g.h) (Node x l r) = fmap g (Node x l r) . fmap g (Node x l r)
  for all binar trees l,r

(i) Left side of equation
fmap (g.h) (Node x l r)
  = Node (fmap g . fmap h x) (fmap (g.h) l) (fmap (g.h) r)
  = Node (g (h x)) (fmap (g.h) l) (fmap (g.h) r) 

(ii) Right side of equation
fmap g (Node x l r) . fmap h (Node x l r)
  = fmap g (Node (h x) (fmap h l) (fmap h r))   
  = (Node (g (h x)) (fmap g (fmap h l) (fmap g (fmap h r)
  = (Node (g (h x)) (fmap (g.h) l) (fmap (g.h) r) 


So again, both sides of the equation are equal for the tree of form (Node x l r)

By the principle of structural induction over binary trees
The claim: (tf.2) fmap (g.h) = fmap g . fmap h
  is True
QED proof of (tf.2)


