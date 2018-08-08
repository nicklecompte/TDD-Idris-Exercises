data SnocList : List a -> Type where
    Empty : SnocList []
    Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListBuilder : (snoc : SnocList start) -> (remainder : List a) -> SnocList (start ++ remainder)
snocListBuilder {start = start} snoc [] = rewrite appendNilRightNeutral start in snoc
snocListBuilder {start = start} snoc (x :: xs) = rewrite appendAssociative start [x] xs in snocListBuilder (Snoc snoc {x}) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListBuilder Empty xs

myReverseSnocHelper : (input : List a) -> SnocList input -> List a
myReverseSnocHelper [] Empty = []
myReverseSnocHelper (xs ++ [x]) (Snoc rec) = x :: (myReverseSnocHelper xs rec)

myReverse : List a -> List a
myReverse xs = myReverseSnocHelper xs (snocList xs)

myReverse2 : List a -> List a
myReverse2 input with (snocList input)
    myReverse2 [] | Empty = []
    myReverse2 (xs ++ [x]) | (Snoc rec) =  x :: (myReverse2 xs | rec) -- rec is only computed once, in snocList input, so reverse only traverses the list twice.

isSuffix : Eq a => List a -> List a -> Bool
isSuffix maybeSuffix input with (snocList maybeSuffix)
  isSuffix [] input | Empty = True
  isSuffix (xs ++ [x]) input | (Snoc rec) with (snocList input)
    isSuffix (xs ++ [x]) [] | (Snoc rec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc rec) | (Snoc z) = if x == y then isSuffix xs ys | rec | z
                                                                         else False
