total consumeMaybe : Maybe Nat -> Nat
consumeMaybe _ = Z

total thisWorks : Maybe Nat -> Nat
thisWorks (Just a) = a
thisWorks Nothing = Z

total thisAlsoWorks : Maybe Nat -> Nat
thisAlsoWorks (Just a) = a
thisAlsoWorks n @ Nothing = ?ab --consumeMaybe n

-- total thisDoesntWork : Maybe Nat -> Nat
-- thisDoesntWork (Just a) = a
-- thisDoesntWork n @ Nothing = Z -- error is here