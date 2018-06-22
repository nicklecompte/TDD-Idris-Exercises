import Data.Vect

myZipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
myZipWith f [] [] = []
myZipWith f (z :: xs) (w :: ys) = (f z w) :: (myZipWith f xs ys)

addMatrix : Num a =>
              Vect rows (Vect cols a) ->
              Vect rows (Vect cols a) ->
              Vect rows (Vect cols a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = (myZipWith (+) x y) :: (addMatrix xs ys)


transposeHelper : (x : Vect m a) -> (xsTranspose : Vect m (Vect len a)) -> Vect m (Vect (S len) a)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

createEmpties : Vect n (Vect 0 a)
createEmpties = replicate _ []

transposeMatrix : Vect n (Vect m a) -> Vect m (Vect n a)
transposeMatrix [] = createEmpties -- this is a Vect m (Vect 0 a). Whatever m is, there are 0 of them in the input, it's a 0xm vector. So we need an mx0 output.
transposeMatrix (x :: xs) = let xsTranspose = transposeMatrix xs in -- xsTranspose is the last k rows transposed into column.
                            -- Flip the row x and append it as first column in transposeHelper
                            -- this means that each row in xs gets appended with the col value
                            transposeHelper x xsTranspose

dotProduct : Num a => (xs : Vect m a) -> (ys : Vect m a) -> a
dotProduct [] [] = 0
dotProduct xs ys = sum(myZipWith (*) xs ys)

dotProductRow : Num a => (xs : Vect m a) -> (ys : Vect n (Vect m a)) -> Vect n a
dotProductRow xs [] = []
dotProductRow xs (y :: ys) = (dotProduct xs y) :: dotProductRow xs ys

multMatrix_rhs : Num a => (xs : Vect n (Vect m a)) -> (ysTranspose : Vect p (Vect m a)) -> Vect n (Vect p a)
multMatrix_rhs [] ysTranspose = []
multMatrix_rhs (x :: xs) ysTranspose = dotProductRow x ysTranspose :: multMatrix_rhs xs ysTranspose

multMatrix : Num a =>
              Vect n (Vect m a) ->
              Vect m (Vect p a) ->
              Vect n (Vect p a)
multMatrix xs ys = let ysTranspose = transposeMatrix ys in
                   multMatrix_rhs xs ysTranspose
