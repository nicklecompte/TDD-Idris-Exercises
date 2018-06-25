import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Double)

testMatrix : Matrix 2 3
testMatrix = [[0,0,0],[0,0,0]]

-- Exercise 2 is in printf.

TupleVect : Nat -> Type -> Type
TupleVect Z _ = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

test : TupleVect 4 Nat
test = (1,2,3,4,())