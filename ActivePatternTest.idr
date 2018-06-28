fib : Stream Integer -> Stream Integer
fib x with (x)
    | (1::tFib) = 1 :: 1 :: [a + b | (a,b) <- zip fib tFib]


containsTen : List Integer -> Bool
containsTen [] = False
containsTen (x :: []) = (x == 10)
containsTen (x :: y :: xs) with (x + y)
    | 10 = True
    | _ = containsTen (y::xs)

--growHead : List a => List a
--growHead nnl@(x::_) = x::nnl
--growHead [] = []
