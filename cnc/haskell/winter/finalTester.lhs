> data ExprAsn = Val Int | Var String |
>					BinOp (Int -> Int -> Int) ExprAsn ExprAsn |
>					Asn String ExprAsn

> type Env = [(String, Int)]

> fetch :: String -> Env -> Int
> fetch x env = head [b | (a,b) <- env, x == a]

> store :: String -> Int -> Env -> Env
> store var val env = (var, val) : env

> newtype State a = State (Env -> (Env, a))

> instance Functor State where
>	fmap f (State s) = State $ \env -> let
>									(xs, val) = s env
>									in (xs, f val)

> fetchS :: String -> State Int
> fetchS var = State (\env -> (env, fetch var env))

> storeS :: String -> State Int -> State Int
> storeS var (State s) = State (\env -> let (env', val) = s env
>									in (store var val env', val))

> applyS :: State Int -> Env -> (Env, Int)
> applyS (State s) env = s env

> infixl 4 <*>
> class Functor f => Applicative f where
>	pure :: a -> f a 
>	(<*>) :: f (a -> b) -> f a -> f b

> infixl 1 >>>=
> class Applicative f => MonadBind f where
>	(>>>=) :: f a -> (a -> f b) -> f b

> evalSRef :: ExprAsn -> Env -> (Env, Int)
> evalSRef (Val x) env = (env, x)
> evalSRef (Var var) env = (env, fetch var env)
> evalSRef (BinOp op e1 e2) env = (env, (op (snd $ evalSRef e1 env) (snd $ evalSRef e2 env)))
> evalSRef (Asn var e) env = let 
>	(env', x) = evalSRef e env
>	env'' = store var x env'
>	in evalSRef e env''

> instance Applicative State where
>	pure g = State (\env -> (env, g))
>	State eg <*> State ex = State $ \env -> let
>		(env', x) = ex env
>		(env'',g) = eg env'
>		in (env'', (g x))

> evalS :: ExprAsn -> State Int
> evalS (Val x) = pure x
> evalS (Var var) = fetchS var
> evalS (BinOp op e1 e2) = pure op <*> evalS e1 <*> evalS e2
> evalS (Asn var e) = storeS var (evalS e)

storeS var (evalS e) 

> evalSRun :: ExprAsn -> Env -> (Env, Int)
> evalSRun expr env = applyS (evalS expr) env

> instance MonadBind State where
>	State s >>>= f = State $ \env -> let
>		(env', x) = s env
>		State s' = f x
>		in s' env'

> evalSMB :: ExprAsn -> State Int
> evalSMB (Val x) = pure x
> evalSMB (Var var) = fetchS var
> evalSMB (BinOp op e1 e2) = pure op >>>= (\f -> evalSMB e1 
>								>>>= (\x1 -> evalSMB e2
>								>>>= (\x2 -> pure (f x1 x2))))
> evalSMB (Asn var e) = storeS var (evalSMB e)

> evalSMBRun :: ExprAsn -> Env -> (Env, Int)
> evalSMBRun expr env = applyS (evalSMB expr) env

> instance Monad State where
>	State s >>= f = State $ \env -> let
>		(env', x) = s env
>		State s' = f x
>		in s' env'
>	return g = State (\env -> (env, g))

> evalSM :: ExprAsn -> State Int
> evalSM (Val x) = pure x
> evalSM (Var var) = fetchS var
> evalSM (BinOp op e1 e2) = do
>	f <- pure op
>	x1 <- evalSM e1
>	x2 <- evalSM e2
>	return (f x1 x2)
> evalSM (Asn var e) = storeS var (evalSM e)

> evalSMRun :: ExprAsn -> Env -> (Env, Int)
> evalSMRun expr env = applyS (evalSM expr) env

	(>>>=) :: f a -> (a -> f b) -> f b

> e1 = Val 3
> e2 = BinOp (+) (Val 3) (Val 4)
> e3 = BinOp (*) (BinOp (-) (Val 9) (Val 4)) (BinOp (-) (Val 7) (Val 2)) 
> e4 = BinOp (+) (BinOp (*) (Val 9) (Val 4)) (BinOp (div) (Val 7) (Val 2)) 
> e5 = BinOp (-) (Val 3) (BinOp (-) (Val 4) (Val 6)) 
> e6 = Var "a" 
> e7 = BinOp (+) (Var "a") (Var "b")
> e8 = BinOp (*) (BinOp (-) (Var "a") (Val 4)) (BinOp (-) (Val 7) (Var "b")) 
> e9 = BinOp (+) (BinOp (*) (Val 9) (Var "a")) (BinOp (div) (Var "b") (Var "c")) 

-- For assignment tests

> e10 = BinOp (+) (BinOp (*) (Asn "a" (Val 6)) (Var "b"))
>               (BinOp (+) (Var "a") (BinOp (*) (Asn "b" (Val 3)) (Var "b")))

> test_evalSMB = [ evalSMBRun e1 [] == evalSRef e1 [] -- should do em all this way
>               ,evalSMBRun e2 [] == evalSRef e2 []
>               ,evalSMBRun e3 [] == evalSRef e3 []
>               ,evalSMBRun e4 [] == evalSRef e4 []
>               ,evalSMBRun e5 [] == evalSRef e5 []
>               ,evalSMBRun e6 [("a",5)] == ([("a",5)], 5)
>               ,evalSMBRun e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
>               ,evalSMBRun e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
>               ,evalSMBRun e9 [("a",7), ("b",8), ("c", 2)]
>                         == ([("a",7), ("b",8), ("c", 2)], 67)
>               ,evalSMBRun e10 [("a",7), ("b",2)] 
>                   == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
>               ]

> test_evalSM = [ evalSMRun e1 [] == evalSRef e1 [] -- should do em all this way
>               ,evalSMRun e2 [] == evalSRef e2 []
>               ,evalSMRun e3 [] == evalSRef e3 []
>               ,evalSMRun e4 [] == evalSRef e4 []
>               ,evalSMRun e5 [] == evalSRef e5 []
>               ,evalSMRun e6 [("a",5)] == ([("a",5)], 5)
>               ,evalSMRun e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
>               ,evalSMRun e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
>               ,evalSMRun e9 [("a",7), ("b",8), ("c", 2)]
>                         == ([("a",7), ("b",8), ("c", 2)], 67)
>               ,evalSMRun e10 [("a",7), ("b",2)] 
>                   == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
>               ]


> test_evalS = [ evalSRun e1 [] == ([], 3)
>              ,evalSRun e2 [] == ([], 7)
>              ,evalSRun e3 [] == ([], 25)
>              ,evalSRun e4 [] == ([], 39)
>              ,evalSRun e5 [] == ([], 5)
>              ,evalSRun e6 [("a",5)] == ([("a",5)], 5)
>              ,evalSRun e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
>              ,evalSRun e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
>              ,evalSRun e9 [("a",7), ("b",8), ("c", 2)]
>                          == ([("a",7), ("b",8), ("c", 2)], 67)
>              ,evalSRun e10 [("a",7), ("b",2)] 
>                          == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
>              ]

-- Add more assignment tests

> test_evalS_fail =
>  evalSRun e9 [("a",7), ("b",8)]

> test_evalSRef = [ evalSRef e1 [] == ([], 3)
>                 ,evalSRef e2 [] == ([], 7)
>                 ,evalSRef e3 [] == ([], 25)
>                 ,evalSRef e4 [] == ([], 39)
>                 ,evalSRef e5 [] == ([], 5)
>                 ,evalSRef e6 [("a",5)] == ([("a",5)], 5)
>                 ,evalSRef e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
>                 ,evalSRef e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
>                 ,evalSRef e9 [("a",7), ("b",8), ("c", 2)]
>                          == ([("a",7), ("b",8), ("c", 2)], 67)
>                 ,evalSRun e10 [("a",7), ("b",2)]
>                           == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
>                 ]

-- Add assignment tests

> test_evalSRef_fail =
>  evalSRef e9 [("a",7), ("b",8)]

> test_all = and test_evalSRef &&
>           and test_evalS &&
>           and test_evalSMB &&
>           and test_evalSM





