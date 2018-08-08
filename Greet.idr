import InfIO

greetInfIO : InfIO
greetInfIO  = do putStrLn "Enter your name: "
                 name <- getLine
                 putStrLn ("Hello " ++ name ++ "!")
                 greetInfIO

data RunIO : Type -> Type where
    Quit : a -> RunIO a
    Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b                  