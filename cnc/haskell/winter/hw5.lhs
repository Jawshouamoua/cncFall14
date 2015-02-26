Part I


> data Exp = Var String | Val Int | Add Exp Exp
>	deriving Show

> type Env = [(String, Int)]

> fetch x []  = error "Can't fetch from empty list"
> fetch x env = head [b | (a,b) <- env, x == a]

> eval :: Exp -> Env -> Int
> eval (Var x) env = fetch x env
> eval (Val x) env = x
> eval (Add e1 e2) env = eval e1 env + eval e2 env

> infixl 4 <*>
> class Functor f => Applicative f where
>	pure :: a -> f a
>	(<*>) :: f (a -> b) -> f a -> f b

> instance Applicative ((->) env) where
>	pure g = \env -> g
>	eg <*> ex = \env -> (eg env) (ex env)

> evalA :: Exp -> Env -> Int
> evalA (Var x) = fetch x
> evalA (Val x) = pure x
> evalA (Add e1 e2) = pure (+) <*> evalA e1 <*> evalA e2

> t1 = Val 3
> t2 = Add (Val 3) (Val 4)
> t3 = Var "a"
> t4 = Var "b"
> t5 = Add t3 t4

> test_eval = [ eval t1 [] == 3,
>				eval t2 [] == 7,
>				eval t3 env1 == 5,
>				eval t4 env1 == 7,
>				eval t5 env1 == 12 ]

> test_evalA = [ evalA t1 [] == 3,
>				evalA t2 [] == 7,
>				evalA t3 env1 == 5,
>				evalA t4 env1 == 7,
>				evalA t5 env1 == 12 ]

> env1 = [("a",5), ("b",7), ("c",2), ("d", 11)]

Part II

> data Expr = Val' Int 
>		| Var' String |
>		BinOp (Int -> Int -> Int) Expr Expr

> eval' :: Expr -> Env -> Maybe Int
> eval' (Val' x) env = Just x
> eval' (Var' x) env = lookup x env
> eval' (BinOp op e1 e2) env  
>	| (eval' e1 env) == Nothing || (eval' e2 env) == Nothing	= Nothing
>	| otherwise = Just z
>		where
>			z = op (newEval (eval' e1 env)) (newEval (eval' e2 env))

> newEval (Just x) = x

 evalA' :: Expr -> Env -> Maybe Int
 evalA' (Val' x) = pure (Just x)
 evalA' (Var' x) = lookup x 
 evalA' (BinOp op e1 e2) 
	| evalA' e1 <*> evalA' e2 == Nothing	= Nothing
	| otherwise = Just z
		where
			z = pure op <*> newEval $ evalA' e1 <*> newEval $ evalA' e2

 evalA' (BinOp op e1 e2) = fmap (pure op) (evalA' e1) <*> evalA' e2

> t6 = Val' 3
> t7 = Val' 2
> t8 = (BinOp (+) t6 t7) 
> t9 = Var' "a"
> t10 = Var' "d"

> testEval' = [eval' t6 [] == Just 3,
>				eval' t7 [] == Just 2,
>				eval' t8 [] == Just 5,
>				eval' t9 env1 == Just 5,
>				eval' t10 env1 == Just 11]


> lift :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
> lift _ Nothing _ = Nothing
> lift _ _ Nothing = Nothing
> lift f (Just x1) (Just x2) = Just (f x1 x2) 

> evalMRef2 :: Expr -> Env -> Maybe Int
> evalMRef2 (Val' x) = pure (Just x)
> evalMRef2 (Var' x) = lookup x
> evalMRef2 (BinOp op e1 e2) = pure (lift op) <*> evalMRef2 e1 <*> evalMRef2 e2


> testEvalMRef2 = [evalMRef2 t6 [] == Just 3,
>				evalMRef2 t7 [] == Just 2,
>				evalMRef2 t8 [] == Just 5,
>				evalMRef2 t9 env1 == Just 5,
>				evalMRef2 t10 env1 == Just 11]

> instance Applicative Maybe where
>	pure = Just
>	Nothing <*> _ = Nothing
>	(Just g) <*> x = fmap g x	

> lift2 :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
> lift2 g x y = pure g <*> x <*> y

> evalM :: Expr -> Env -> Maybe Int
> evalM (Val' x) = pure (pure x)
> evalM (Var' x) = lookup x
> evalM (BinOp op e1 e2) = pure (pure op) <*> evalM e1 <*> evalM e2
