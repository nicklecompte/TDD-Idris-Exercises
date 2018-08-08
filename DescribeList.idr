describeList : List Int -> String
describeList [] = "empty"
describeList (x::xs) = "Non-empty, tail is " ++ show xs

data ListLast : List a -> Type where
    Empty : ListLast []
    NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] (Empty) = "empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non empty, initial portion is " ++ show xs

total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          (NonEmpty ys y) => NonEmpty (x::ys) y 
                           

describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)

describeListEendConcise : List Int -> String
describeListEendConcise xs with (listLast xs)
  describeListEendConcise [] | Empty = "Empty"
  describeListEendConcise (ys ++ [x]) | (NonEmpty ys x) = "Non empty, initial portion is " ++ show ys

myReverse: List a -> List a
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (NonEmpty xs x) = x :: (myReverse xs)

data SplitList : List a -> Type where
    SplitNil : SplitList []
    SplitOne : SplitList [x]
    SplitPair : (left: List a) -> (right : List a) -> SplitList (left ++ right)

splitList : (input : List a) -> SplitList input
splitList input = splitListHelper input input
    where
        splitListHelper : List a -> (input : List a) -> SplitList input
        splitListHelper _ [] = SplitNil
        splitListHelper _ [x] = SplitOne
        splitListHelper (_ :: _ :: counter) (item :: items)
            = case splitListHelper counter items of
                SplitNil => SplitOne
                SplitOne {x} => SplitPair [item] [x]
                SplitPair left right => SplitPair (item::left) right
        splitListHelper _ items = SplitPair [] items

mergeSort: Ord a => List a -> List a
mergeSort input with (splitList input)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (left ++ right) | (SplitPair left right) = merge (mergeSort left) (mergeSort right)
data TakeN : List a -> Type where
    Fewer : TakeN xs
    Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

takeN : (n: Nat) -> (xs: List a) -> TakeN xs
takeN Z _ = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) with (takeN k xs)
  takeN (S k) (x :: xs) | Fewer = Fewer
  takeN (S k) (x :: (n_xs ++ rest)) | (Exact n_xs) = Exact (x :: n_xs)

groupByN : (n: Nat) -> (xs: List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: (groupByN n rest)

halves: List a -> (List a, List a)
halves xs with (takeN (div (length xs) 2) xs)
  halves xs | Fewer = ([],xs)
  halves (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)
