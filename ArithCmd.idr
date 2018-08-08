import Data.Primitives.Views
import System

%default total
%access public export

data Command : Type -> Type where
    PutStr : String -> Command ()
    GetLine : Command String

data ConsoleIO : Type -> Type where
    Quit : a -> ConsoleIO a
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do    

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

total
run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)
run Dry x = do pure Nothing                            

quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
quiz (num1::num2::nums) score
    = do    PutStr ("Score so far: " ++ show score ++ "\n")                                        
            PutStr (show num1 ++ " * " ++ show num2 ++ " =? ")
            answer <- GetLine
            if (toLower answer == "quit")
                then Quit score else
            if (cast answer == num1 * num2)
                then do PutStr "Correct! \n"
                        quiz nums (score + 1)
                else do PutStr ("Wrong, the answer is " ++ show (num1 * num2) ++ "\n")
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

partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
              | Nothing => putStrLn "Ran out of fuel"            
          putStrLn ("Final score: " ++ (show score))