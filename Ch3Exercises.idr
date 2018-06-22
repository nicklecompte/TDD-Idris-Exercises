import Data.Vect

my_length : List a -> Nat
my_length [] = Z
my_length (x :: xs) = S(my_length xs)

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = (my_reverse xs) ++ [x]

my_list_map : (a -> b) -> List a -> List b
my_list_map _ [] = []
my_list_map f (x :: xs) = (f x) :: (my_list_map f xs)
-- here, hitting Ctrl-Alt-S gave my_list_map f (x::xs) = []
-- this is okay from a type perspective but obviously wrong from logic.

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs
-- here, hitting Ctrl-Alt-S gave the right answer :)
-- returning f (x :: xs) = [] would have violated the type,
-- since the output length has to be n.
-- Since there's only one named argument that f can apply to (x),
-- and then recursion is a reasonablke choice from there.
-- Note that maybe this wouldn't have worked!
-- Maybe we need to do f(x :: y :: xs) for some other algorithm
-- and f(x + y) :: other xs was correct. But Idris picked the correct
-- most obvious answer.
