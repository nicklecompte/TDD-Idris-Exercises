import Data.Primitives.Views
import System

%default total

data InfIO : Type where
    Do :    IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do     

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run Dry _ = putStrLn "Out of fuel"
run (More fuel) (Do action cont) = do res <- action
                                      run fuel (cont res)

quiz : Stream Int -> (score : Nat) -> InfIO
quiz (num1::num2::nums) score
    = do    putStrLn ("Score so far: " ++ show score)                                        
            putStr (show num1 ++ " * " ++ show num2 ++ " =? ")
            answer <- getLine
            if (cast answer == num1 * num2)
                then do putStrLn "Correct!"
                        quiz nums (score + 1)
                else do putStrLn ("Wrong, the answer is " ++ show (num1 * num2))
                        quiz nums score

psuedoRandoms : Int -> Stream Int
psuedoRandoms seed = let seed' = 1664525 * seed  + 1013904223 in
                            (seed' `shiftR` 2) :: psuedoRandoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (psuedoRandoms seed)
    where
        bound : Int -> Int
        bound num with (divides num 12)
            bound ((12 * div) + rem) | (DivBy prf) = rem + 1                        

partial -- needs to be marked partial because this function does not terminate.
forever : Fuel
forever = More forever                        

partial -- cf above. When you run exec, the fact that you marked total means that any failure of the program to terminate
main : IO () -- is EXCLUSIVELY because you told it to run forever. I.e. it will never get hung up elsewhere on the back end.
main = do seed <- time
          run forever (quiz (arithInputs (fromInteger seed)) 0) 