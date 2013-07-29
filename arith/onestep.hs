data Term = Ttrue | Tfalse | Zero
          | Succ Term | Pred Term | IsZero Term
          | If Term Term Term
          | Terror String
          deriving (Show)

isNumericVal :: Term -> Bool
isNumericVal Zero = True
isNumericVal (Succ t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal Ttrue = True
isVal Tfalse = True
isVal t = isNumericVal t


eval1 :: Term -> Term

eval1 Zero = Zero
eval1 Ttrue = Ttrue
eval1 Tfalse = Tfalse
eval1 (Terror x) = Terror x

eval1 (Succ t) =
    if isVal t
    then Succ t
    else Succ (eval1 t)

eval1 (Pred Zero) = Zero
eval1 (Pred (Succ nv)) = nv
eval1 (Pred t) =
    let t' = eval1 t
    in eval1 (Pred t')

eval1 (IsZero Zero) = Ttrue
eval1 (IsZero t) =
    if isNumericVal t
    then Tfalse
    else eval1 (IsZero (eval1 t))

eval1 (If Ttrue t2 _) = eval1 t2
eval1 (If Tfalse _ t3) = eval1 t3
eval1 (If t1 t2 t3) =
    let t1' = eval1 t1
    in eval1 (If t1' t2 t3)

eval1 t = Terror ("Stuck on " ++ show t)


evalB :: Term -> Term
evalB Zero = Zero
evalB Ttrue = Ttrue
evalB Tfalse = Tfalse

evalB (IsZero t) =
    let t' = evalB t
    in case t' of
        Terror x -> Terror x
        Zero -> Ttrue
        _ -> Tfalse

evalB (Succ t) =
    let t' = evalB t
    in case t' of
        Terror x -> Terror x
        Zero -> Succ Zero
        Succ t -> Succ (Succ t)
        _ -> Terror (show t ++ " is not a numerical")

evalB (Pred t) =
    let t' = evalB t
    in case t' of
        Terror x -> Terror x
        Zero -> Zero
        Succ t -> t
        _ -> Terror (show t ++ " is not a numerical")

evalB (If t1 t2 t3) =
    let t1' = evalB t1
    in case t1' of
        Ttrue  -> evalB t2
        Tfalse -> evalB t3
        Terror x -> Terror x
        t -> Terror (show t ++ " is not a boolean")

evalB (Terror x) = Terror x




case1 = (If (IsZero (Succ (Succ Zero)))
            (Succ Zero)
            (Pred Zero))

case2 = (IsZero (Succ (Pred Zero)))

case3 = (If Ttrue
            (IsZero Zero)
            Zero)

case4 = Ttrue
case5 = Tfalse
case6 = Zero

check :: Int -> Term -> IO ()
check n t = do
    putStrLn ("Case " ++ show n ++ " " ++ show t)
    putStrLn $ "  onestep: " ++ (show $ eval1 t)
    putStrLn $ "  bigstep: " ++ (show $ evalB t)

main = do
    check 1 case1
    check 2 case2
    check 3 case3
    check 4 case4
    check 5 case5
    check 6 case6
