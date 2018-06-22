import Data.Vect

-- We use a define-refine approach to
-- figure out how to implement insertion sort...
-- Insertion sort: given head and tail,
-- recursively sort the tail and then insert the head.

insert : Ord elem => (x : elem) -> (xsSorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs
                              else y :: insert x xs -- since xs will be sorted tpo

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

{-
After hitting ctrl-alt-A on insSort we get
  insSort [] = ?insSort_rhs_1
  insSort (x :: xs) = ?insSort_rhs_2

First of all, sorting an empty list returns the empty list.
We also get this from the type. So you can hit Ctrl-Alt-S
on ?insSort_rhs_1 to search for the only expression that fits.
Unfortunately doing that on ?insSort_rhs_2 gives insSort (x :: xs) = x :: xs
- not exactly what we wanted.
We know we need to sort the list, so we can go ahead and bind that to a Let:
insSort (x :: xs) = let xsSorted = insSort xs in
                        ?insSort_rhs_2
insSort_rhs_2 has to at some point insert x into xs for the algorithm to work.
So we can define a function
insSort_rhs_2 : (x : elem) -> (xs: Vect k elem) -> Vect k elem
-}

--insSort [] = []
--insSort (x :: xs) = let xsSorted = insSort xs in
--                        ?insSort_rhs_2
