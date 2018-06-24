import Data.Fin

data Vect : Nat -> Type -> Type where
    Nil : Vect Z a
    (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] y = y
append (x :: xs) y = x :: append xs y

zip : Vect n a -> Vect n b -> Vect n (a,b)
zip [] y = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

vectTake : (m : Nat) -> Vect (m + n) a -> Vect m a
vectTake Z x = []
vectTake (S k) (x :: xs) = x :: vectTake k xs
-- we can also use dynamic types to make a completely safe indexing function:
--
index : Fin n -> Vect n a -> a
index FZ [x] = x -- FZ is actually 1.
index (FS k) (x::xs) = index k xs

sumEntries : Num a => (ind: Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} ind x y = case (integerToFin ind n) of
                                Nothing => Nothing
                                Just k => Just ((index k x) + (index k y))
