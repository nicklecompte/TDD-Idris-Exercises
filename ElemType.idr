import Data.Vect

data Elem : a -> Vect k a -> Type where
    Here : Elem x (x :: xs)
    There : (later : Elem x xs) -> Elem x (y :: ys)

isElem     