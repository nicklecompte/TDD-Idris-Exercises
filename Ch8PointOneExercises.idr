||| A proof that, given two equal lists, appending the same value gives the same result
same_cons : {xs : List a} -> {ys : List a} -> (xs = ys) -> (x :: xs = x :: ys)
same_cons Refl = Refl -- that's all we need :) since this typechecks and is total, it works.
                      -- Specifically, we are given a proof that two lists are equal and this has to be Refl.
                      -- OOH, the Idris type system restricts the only possible answer to Refl.
                      -- OTOH, x :: xs = x :: ys uses x = x  as an implicit composition of Refls. See this exercise for why:

||| Here you need a proof x = y first to infer.                      
same_lists : {xs : List a } -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
    Same : ThreeEq a a a

allSameS : (x,y,z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z Same = Same
