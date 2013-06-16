data Term = Ttrue | Tfalse | Zero
            | Succ Term | Pred Term | IsZero Term
            | If Term Term Term
            deriving (Show)

data Value = Vtrue | Vfalse | Vinteger Integer | Verror String
             deriving (Show)

evaluate :: Term -> Value

evaluate Ttrue = Vtrue
evaluate Tfalse = Vfalse
evaluate Zero = Vinteger 0

evaluate (Succ t) =
    let r = evaluate t
    in case r of
        Vinteger a -> Vinteger (a + 1)
        _ -> Verror (show t ++ " is not a integer")

evaluate (Pred t) =
    let r = evaluate t
    in case r of
        Vinteger a -> Vinteger (a - 1)
        _ -> Verror (show t ++ " is not a integer")

evaluate (IsZero t) =
    let r = evaluate t
    in case r of
        Vinteger 0 -> Vtrue
        _ -> Vfalse

evaluate (If t1 t2 t3) =
    let r = evaluate t1
    in case r of
        Vtrue -> evaluate t2
        Vfalse -> evaluate t3
        _ -> Verror (show t1 ++ " is not a boolean")

repr :: Value -> String
repr Vtrue = "True"
repr Vfalse = "False"
repr (Vinteger a) = show a
repr (Verror s) = "Error: " ++ s

main = do
    -- case 1
    putStrLn $ repr $ evaluate
            (If (IsZero (Succ (Succ Zero)))
                (Succ Zero)
                (Pred Zero))
    -- case 2
    putStrLn $ repr $ evaluate
            (IsZero (Succ (Pred Zero)))
    -- case 3
    putStrLn $ repr $ evaluate
            (If Ttrue
                (IsZero Zero)
                Zero)

