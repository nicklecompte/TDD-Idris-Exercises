module WordLength

allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = (length x) :: allLengths xs

-- Good idea to check that this is total - that is, guaranteed to run without Idris errors.
-- in REPL, :total allLengths confirms this.

-- Let's define this in an interactive, type-driven way.
||| Hole-based implementation of the allLengths thingy, for an exercise.
allLengthsTD : List String -> List Nat
-- First let's give an "abstract" implementation using holes.
allLengthsTD [] = ?allLengths_1
allLengthsTD (x :: xs) = ?allLengths_2 xs
{-
in Atom, hitting Ctrl-Alt-T with cursor on ?allLengths gives
allLengths : List String -> List Nat
Hit Ctrl-Alt-C on xs to give all the possible forms Idris recognizes xs can take.
xs is a list so those forms are [] or x :: xs
then you can fill in the ?allLengths_1 and ?allLengths_2 holes
in a way that respects the business logic.
if you have an empty list of strings
-}
-- Each data type has one or more constructors, which are primitive ways of
-- building values in that datatype.

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)
