-- https://github.com/idris-lang/Idris-dev/issues/4483
%default total
%hide Semigroup

record Semigroup where
  constructor MkSemigroup'
  Set : Type
  Op : Set -> Set -> Set
  Assoc : (x,y,z : Set) -> Op x (Op y z) = Op (Op x y) z

MkSemigroup : (set : Type) -> (op : set -> set -> set) -> {auto assoc : (x,y,z : set) -> op x (op y z) = op (op x y) z} -> Semigroup
MkSemigroup set op {assoc} = MkSemigroup' set op assoc

myPlus : Nat -> Nat -> Nat
myPlus x y = x + y

%hint
plusAssociate : (x,y,z : Nat) -> myPlus x (myPlus y z) = myPlus (myPlus x y) z
plusAssociate = plusAssociative

NatSemigroup : Semigroup
NatSemigroup = MkSemigroup Nat myPlus