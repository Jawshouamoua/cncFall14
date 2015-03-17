> import Control.Monad.Instances

> data Expr = Var String | Val Int | BinOp (Int -> Int -> Int) Expr Expr

> type Env = [(String, Int)]

> newtype Arrow r a = Arrow (r -> a)

> instance Monad (Arrow r) where
>	return g = Arrow $ \env -> g
>	Arrow ex >>= eg = Arrow $ \env -> let
>		x = ex env
>		Arrow x' = eg x
>		in x' env

> fetch x [] = error "Can't fetch from empty list"
> fetch x env = head [b | (a,b) <- env, x == a]

> eval :: Expr -> Env -> Int
> eval (Var x) = fetch x 
> eval (Val x) = return x
> eval (BinOp op e1 e2) = do
>	x1 <- eval e1
>	x2 <- eval e2
>	return (op x1 x2) 

> eval' :: Expr -> Env -> Maybe Int
> eval' (Val x) = return (Just x)
> eval' (Var x) = lookup x
> eval' (BinOp op e1 e2) = do
>	x1 <- e1
>	x2 <- e2
>	val <- Just op <*> e1 <*> e2
>	return val

