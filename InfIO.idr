module InfIO

%access public export

data InfIO : Type where
    Do :    IO a
            -> (a -> Inf InfIO)
            -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do             

loopPrint : String -> InfIO
loopPrint msg = do (putStrLn msg) -- "do" is syntactic sugar for the >>= operator, not a monad shortcut, so we can drop it in w/out defining Monad implementation.
                   loopPrint msg

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

run : Fuel -> InfIO -> IO ()
run Dry _ = putStrLn "Out of fuel"
run (More fuel) (Do action cont) = do res <- action
                                      run fuel (cont res)

total
totalREPL : (prompt : String) -> (action: String -> String) -> InfIO
totalREPL prompt action = do putStrLn (action prompt)
                             putStrLn "Input: "
                             newPrompt <- getLine
                             totalREPL newPrompt action
                                      