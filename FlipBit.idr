import Data.Vect


data Parity = Even | Odd

flipParity : Parity -> Parity
flipParity Even = Odd
flipParity Odd = Even


record Bitflags where
  constructor MkFlags

  f0 : Bool
  f1 : Bool
  f2 : Bool
  f3 : Bool

go : List Bool -> Parity
go [] = Even
go (False :: xs) = go xs
go (True :: xs) = flipParity (go xs)

flipHead : List Bool -> List Bool
flipHead [] = [True]
flipHead (x::xs) = (not x) :: xs

goLemma : (list : List Bool) -> flipParity (go list) = go (flipHead list)
goLemma [] = Refl
goLemma [x] = Refl
--goLemma (x :: xs) = let p = go xs in


parity : Bitflags -> Parity
parity (MkFlags f0 f1 f2 f3) = go [f0, f1, f2, f3]


data MyData : (p : Parity) -> Type where
  MkData : (flags : Bitflags) ->
           (prf : parity flags = p) ->
           MyData p

defaultData : MyData Even
defaultData = MkData (MkFlags False False False False) Refl

data Field = F0 | F1 | F2 | F3

flipBit : Field -> Bitflags -> Bitflags
flipBit F0 flags = record { f0 $= not } flags
flipBit F1 flags = record { f1 $= not } flags
flipBit F2 flags = record { f2 $= not } flags
flipBit F3 flags = record { f3 $= not } flags


flipProof : (field : Field) ->
            (flags : Bitflags) ->
            flipParity (parity flags) = parity (flipBit field flags)
flipProof F0 (MkFlags p1 p2 p3 p4) = ?flipProof_rhs_1
flipProof F1 (MkFlags p1 p2 p3 p4) = ?flipProof_rhs_2
flipProof F2 (MkFlags p1 p2 p3 p4) = ?flipProof_rhs_3
flipProof F3 (MkFlags p1 p2 p3 p4) = ?flipProof_rhs_4


flipField : {inputParity : Parity} ->
            Field ->
            MyData inputParity ->
            MyData (flipParity inputParity)
flipField field (MkData flags prf) = MkData (flipBit field flags) (sym $ rewrite sym prf in flipProof field flags)