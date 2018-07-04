data Elem : (value : a) -> (xs : List a) -> Type where
    Here : Elem x (x::xs) -- proof that x is the first element of a vector
    There : (later : Elem x xs) -> Elem x (y :: xs) -- proof that if x is in xs, then x must be in y::xs

data Last : List a -> a -> Type where
    LastOne : Last [value] value
    LastCons : (prf: Last xs value) -> Last (x :: xs) value

notInEmptyList : Last [] a -> Void    
notInEmptyList LastOne impossible
notInEmptyList (LastCons _) impossible

notInDistinctSingleton : ((a=b) -> Void) -> Last [a] b -> Void
notInDistinctSingleton contra LastOne = contra Refl
notInDistinctSingleton contra (LastCons prf) impossible


notInConsEither : (contra : Last (x::xs) value -> Void) -> (Last (y :: (x :: xs)) value -> Void)
notInConsEither contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)    
isLast [] value = No (notInEmptyList)
isLast [x] value = case decEq x value of
                        (Yes Refl) => Yes LastOne
                        (No contra) => No (notInDistinctSingleton contra)
isLast (y::(x :: xs)) value = case isLast (x::xs) value of
                              (Yes prf) => Yes (LastCons prf)
                              (No contra) => No (notInConsEither contra)
