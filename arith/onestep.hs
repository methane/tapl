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
eval1 (If Ttrue t2 _) = eval1 t2
eval1 (If Tfalse _ t3) = eval1 t3
eval1 (If t1 t2 t3) =
    let t1' = eval1 t1
    in eval1 (If t1' t2 t3)

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

eval1 (Succ t) =
    if isVal t
    then Succ t
    else Succ (eval1 t)

eval1 t = Terror ("Stuck on " ++ show t)

-- repr :: Value -> String
-- repr Ttrue = "True"
-- repr Tfalse = "False"
-- repr (Vinteger a) = show a
-- repr (Verror s) = "Error: " ++ s

main = do
    -- case 1
    putStrLn $ show $ eval1
            (If (IsZero (Succ (Succ Zero)))
                (Succ Zero)
                (Pred Zero))
    -- case 2
    putStrLn $ show $ eval1
            (IsZero (Succ (Pred Zero)))
    -- case 3
    putStrLn $ show $ eval1
            (If Ttrue
                (IsZero Zero)
                Zero)

