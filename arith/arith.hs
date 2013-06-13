data Term = Ttrue | Tfalse | Zero
            | Succ Term | Pred Term | IsZero Term
            | If Term Term Term
            | Error String
            deriving (Show)

evaluate :: Term -> Term
evaluate Ttrue = Ttrue
evaluate Tfalse = Tfalse
evaluate Zero = Zero
evaluate (Succ t) = Succ (evaluate t)
evaluate (Pred t) = Pred (evaluate t)
evaluate (IsZero Zero) = Ttrue
evaluate (IsZero t) =
    let r = evaluate t
    in
        case r of
            Zero -> Ttrue
            _ -> Tfalse

evaluate (If t1 t2 t3) =
    let r = evaluate t1
    in
        case r of
            Ttrue -> evaluate t2
            Tfalse -> evaluate t3
            _ -> Error (show t1 ++ " is not a boolean")

repr :: Term -> String
repr Ttrue = "True"
repr Tfalse = "False"
repr Zero = "0"
repr a = show $ reprInt a

reprInt :: Term -> Int
reprInt Zero = 0
reprInt (Succ t) = reprInt t + 1
reprInt (Pred t) = reprInt t - 1

main = do
    putStrLn $ repr $ evaluate
            (If (IsZero (Succ (Succ Zero)))
                (Succ Zero)
                (Pred Zero))
    putStrLn $ repr $ evaluate
            (IsZero (Succ (Pred Zero)))
    putStrLn $ repr $ evaluate
            (If Ttrue
                (IsZero Zero)
                Zero)
