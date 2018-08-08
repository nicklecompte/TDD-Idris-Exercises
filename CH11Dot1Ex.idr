import Data.Primitives.Views

every_other : Stream a -> Stream a 
every_other (w::(x::rest)) = x :: (every_other rest)

data InfList : Type -> Type where
    (::) : (value : elem) -> (Inf (InfList elem)) -> InfList elem

Functor InfList where
  map func (value::xs) = (func value) :: (map func xs)

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

getPrefix : (count: Nat) -> InfList ty -> List ty
getPrefix Z x = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs -- xs is auto-forced by the compiler...and Force-ing it doesn't seem to work :/ is this a bug?

psuedoRandoms : Int -> Stream Int
psuedoRandoms seed = let seed' = 1664525 * seed  + 1013904223 in
                         (seed' `shiftR` 2) :: psuedoRandoms seed'

data Face = Heads | Tails

total
getFace : Int -> Face
getFace x with (divides x 2)
  getFace ((2 * div) + rem) | (DivBy prf)
       = case rem of
              0 => Heads
              _ => Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips k xs = map getFace (take k xs)

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = let next = (approx + (number / approx)) / 2 in
                                       next :: (square_root_approx number next)

square_root_bound : (maxIterations : Nat) -> (number: Double ) -> (errorBound : Double) -> (approximations: Stream Double) -> Double                                       
square_root_bound Z number errorBound (value :: xs) = value
square_root_bound (S k) number errorBound (value :: xs) = if (abs ((value*value) - number)) < abs errorBound 
                                                            then value
                                                            else square_root_bound k number errorBound xs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000001 (square_root_approx number number)                                                        

data InfIO : Type where
  Do :    IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do     