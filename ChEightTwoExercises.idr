import Data.Vect

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite (myPlusCommutes k m) in ((plusSuccRightSucc m k))

{-
Earlier implementation of myReverse is inefficient because iut neesd to traverse the entire vector in order to append a single element.
You can write a better definition with an accumulator internal helper function - just needs some proofs :)
-}


reverseProofEmpty : Vect n a -> Vect (plus n 0) a
reverseProofEmpty {n} xs = rewrite (plusZeroRightNeutral n) in xs

reverseProof_xs : Vect (S (plus n m)) a -> Vect (plus n (S m)) a
reverseProof_xs {n} {m} xs = rewrite sym (plusSuccRightSucc n m) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
    where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
          reverse' acc [] = reverseProofEmpty acc
          reverse' acc (x :: xs) = reverseProof_xs (reverse' (x::acc) xs)