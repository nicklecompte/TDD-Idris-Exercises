import Data.Fin

data Vect : Nat -> Type -> Type where
    Nil : Vect Z a
    (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

{-    
append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] y = y
append (x :: xs) y = x :: append xs y
-}

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
 
(Eq a) => Eq (Vect n a) where                                
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = (x == y) && (xs == ys)
 
Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (x :: xs) = foldr func (func x init) xs
  foldl func init [] = init
  foldl func init (x :: xs) = func (foldl func init xs) x   

||| Data of type EqNat num1 num2 implies num1 = num 2 since Same is only valid constructor.
||| The type EqNat 3 4 can be defined but it is empty: there are no values of this type in the entire program.
data EqNat :  (num1 : Nat) -> (num2 : Nat) -> Type where
    Same : (num: Nat) -> EqNat num num

sameS : (k : Nat) -> (j: Nat) -> (EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              (Just x) => Just (sameS k j x)


exactLength : (len : Nat) -> (input : Vect m a) -> Maybe(Vect len a)
exactLength {m} len input = case (checkEqNat m len) of
                                Nothing => Nothing
                                Just (Same len) => Just (input)

                             

checkEqNatBuiltin : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNatBuiltin Z Z = Just Refl
checkEqNatBuiltin Z (S k) = Nothing
checkEqNatBuiltin (S k) Z = Nothing
checkEqNatBuiltin (S k) (S j) = case (checkEqNatBuiltin k j) of
                                     Nothing => Nothing
                                     (Just proof_kEqualsj) => Just (cong proof_kEqualsj)

(++) : Vect a elem -> Vect b elem -> Vect (a+b) elem
(++) [] y = y
(++) (x::xs) y = x :: (xs ++ y)

reverse : Vect n elem -> Vect n elem
reverse [] = []
reverse {n = S k} (x :: xs) 
    = let revSublist = reverse xs 
          result = revSublist ++ [x] in
          rewrite plusCommutative 1 k in result
                                     
reverseProof : (x : elem) -> (xs : Vect k elem) -> Vect (k + 1) elem -> Vect (S k) elem
reverseProof {k} x xs reversedXs = rewrite plusCommutative 1 k in reversedXs

reverseF : Vect a elem -> Vect a elem
reverseF [] = []
reverseF (x :: xs) = reverseProof x xs (reverseF xs ++ [x])

appendToEmpty : (y : Vect m elem) -> Vect (plus m 0) elem
appendToEmpty {m} y = rewrite plusZeroRightNeutral m in y

appendToFull : Vect (S (m + k)) elem -> Vect (plus m (S k)) elem
appendToFull {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs

append : Vect n elem -> Vect m elem -> Vect (m + n) elem -- (in Nat, addition isn't necessarily commutative)
append [] y = appendToEmpty y
append (x :: xs) y = appendToFull (x :: append  xs y)

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> 
    (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys) -> Void)
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> 
    (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys) -> Void)
tailUnequal contra Refl = contra Refl    

DecEq a => DecEq (Vect n a) where
    decEq [] [] = Yes Refl
    decEq (x :: xs) (y :: ys) = case decEq x y of
                                    Yes Refl => case decEq xs ys of
                                                Yes Refl => Yes Refl
                                                No contra => No (tailUnequal contra)
                                    No contra => No (headUnequal contra)

data Elem : (value : a) -> (xs : Vect k a) -> Type where
    Here : Elem x (x::xs) -- proof that x is the first element of a vector
    There : (later : Elem x xs) -> Elem x (y :: xs) -- proof that if x is in xs, then x must be in y::xs

--Uninhabited (Elem value []) where
--    uninhabited Refl impossible        

removeElem : DecEq a => (value : a) -> 
                        (y : Vect (S n) a) -> 
                        (prf: Elem value y) ->
                        Vect n a
removeElem value (value :: xs) Here = xs
removeElem {n = Z} value (x :: []) (There later) = absurd later
removeElem {n = (S k)} value (x :: xs) (There later) = x :: (removeElem value xs later)


maryInVector : Elem "Mary" ["Peter","Paul","Mary"]       
maryInVector = There (There Here)                         