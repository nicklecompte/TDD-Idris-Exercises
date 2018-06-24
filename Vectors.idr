import Data.Vect

fourInts : Vect 4 Int
fourInts = [0,1,2,3]

sixInts : Vect 6 Int
sixInts = [1,2,3,4,5,6]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts

allLengthsVect : Vect len String -> Vect len Nat
allLengthsVect [] = []
allLengthsVect (x :: xs) = (length x) :: allLengthsVect xs

{-
Contrast this with
allLengths : List string -> List string
allLengths xs = []
This compiles fine but doesn't work as the developer surely expected.
By doing
allLengthsVect : Vect len String -> Vect len Nat
we force the output to be the same length as the input, so this woudln't compile.
-}

-- allLengthsVectWrong : Vect len String -> Vect len Nat
-- allLengthsVectWrong xs = []

{-

Idris yells at you when you try to build this:
at line 27, character 26, file C:\Source\TypeDrivenDevelopmentWithIdris\Vectors.idr
When checking right hand side of allLengthsVectWrong with expected type
        Vect len Nat

Type mismatch between
        Vect 0 Nat (Type of [])
and
        Vect len Nat (Expected type)

Specifically:
        Type mismatch between
                0
        and
                len

-}

{-

What if you were to try
-}
allLengthsPartial : Vect len String -> Vect len Nat
allLengthsPartial (x :: xs) = length x :: allLengthsPartial xs

{-
:total allLengthsPartial
Main.allLengthsPartial is not total as there are missing cases
-}

{-

Or explicityl annotate with "total"
 - makes Idris throw a build error if it isn't total.

-}

-- total allLengthsPartial2 : Vect len String -> Vect len Nat
-- allLengthsPartial (x :: xs) = length x :: allLengthsPartial xs

{-
Type checking .\Vectors.idr
Vectors.idr:54:1-62:
   |
54 | allLengthsPartial (x :: xs) = length x :: allLengthsPartial xs
   | ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Main.allLengthsPartial is not total as there are missing cases
-}


-- Idris is smart enough to fill in holes for you:
allLengths3 : Vect n String -> Vect n Nat
allLengths3 [] = [] -- originally this was ?allLengths3_rhs_1
                    -- -> hitting Ctrl-Alt-S did an "expression search"
                    -- and Idris knew that [] is the only vect of length 0
                    -- so [] can be the only possible output of Vect 0 Nat
-- start with allLengths3 (x :: xs) = ?allLengths3_rhs_2 after hitting Ctrl-Alt-C on xs
-- then hit Ctrl-Alt-S on ?allLengyths_3_rhs_2 and see what expression search finds
-- It returns allLengths3 (x :: xs) = 0 :: allLengths3 xs
-- By pattern matching n had to be a Nat of form (S k) and not Z.
-- Therefore the answer had to be at least a nonempty vector.
-- Idris picked 0  for the head as the first possible valid answer
-- and then noticed it could call allLengths3 on the tail to do the rest.
-- The 0 isn't what we want so we fill in with a hole
allLengths3 (x :: xs) = ?allLengths3H ead :: allLengths3 xs
