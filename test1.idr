module Main

mytake : Integer -> (List a) -> List a
mytake  0     _           =  []
mytake  _     []          =  []
mytake  n     (x::xs)     =  x :: mytake (n-1) xs

longer : String -> String -> Nat
longer str1 str2 =
  let len1 = length str1
      len2 = length str2 in
  if len1 > len2 then len1 else len2

pythagorean : Double -> Double -> Double
pythagorean x y = sqrt (square x + square y)
  where
    square : Double -> Double
    square x = x*x

main : IO ()
main = printLn ("hi")
