twoPlusTwoIsNotFive : 2 + 2 = 5 -> Void
twoPlusTwoIsNotFive Refl impossible

loop : Void
loop = loop

valueNotSucc : (x : Nat) -> (x = S x) -> Void
valueNotSucc _ Refl impossible

zeroNotEqualSucc : (0 = S k) -> Void
zeroNotEqualSucc Refl impossible

succNotZero : (S k = 0) -> Void
succNotZero Refl impossible

noEqRhs : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noEqRhs contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec(num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotEqualSucc
checkEqNat (S k) Z = No succNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              (Yes prf) => Yes (cong prf)
                              (No contra) => No (noEqRhs contra)
