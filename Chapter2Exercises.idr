module Chapter2Exercises

palindromeDumb : String -> Bool
palindromeDumb str = (str == reverse str)

palindromCaseInsensitive : String -> Bool
palindromCaseInsensitive str = let strLower = toLower str in
                               strLower == reverse strLower

counts : String -> (Nat,Nat)
counts str = let numWords = length (words str)
                 numChars = length str in
                 (numWords,numChars)

topTen : Ord a => List a -> List a
topTen xs = take 10 (reverse (sort xs))

overLength : Nat -> List String -> Nat
overLength n xs = length (filter (\x => (length x) > n) xs)

calcLength : (lst : List a) -> Nat
calcLength lst = 5
myLength : List a -> Nat
myLength lst = calcLength lst
