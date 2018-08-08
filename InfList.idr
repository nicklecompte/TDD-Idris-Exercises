data InfList : Type -> Type where
    (::) : (value : elem) -> (Inf (InfList elem)) -> InfList elem

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

getPrefix : (count: Nat) -> InfList ty -> List ty
getPrefix Z x = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs -- xs is auto-forced by the compiler...and Force-ing it doesn't seem to work :/ is this a bug?