import Data.Vect
import Data.Vect.Views
import Data.List.Views
import Data.Nat.Views

||| Return the maximum equal suffix of the two input lists. equalSuffix [1,2,3] [1,2,3] = [1,2,3] ;; equalSuffix [1,2,5,6,4,5] [1,2,3,5,4,5] = [4,5]
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (zs ++ [x]) ys | (Snoc rec) with (snocList ys)
    equalSuffix (zs ++ [x]) [] | (Snoc rec) | Empty = []
    equalSuffix (zs ++ [x]) (xs ++ [y]) | (Snoc rec) | (Snoc z) = if (x == y) then (equalSuffix zs xs | rec | z) ++ [x]
                                                                              else []
mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (ys ++ zs) | (SplitRecPair lrec rrec) = merge (mergeSort ys | lrec) (mergeSort zs | rrec)

total
toBinary : Nat -> String
toBinary k with (halfRec k)
    toBinary Z | HalfRecZ = ""
    toBinary (n + n) | (HalfRecEven rec) = toBinary n | rec ++ "0"
    toBinary (S (n + n)) | (HalfRecOdd rec) = toBinary n | rec ++ "1"

total
palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) = if (x == y) then palindrome ys | rec
                                                            else False
