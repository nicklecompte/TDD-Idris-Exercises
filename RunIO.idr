module RunIO

%access public export

data RunIO : Type -> Type where
    Quit : a -> RunIO a
    Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

greet : RunIO ()
greet = do putStrLn "Enter your name: "
           name <- getLine
           if name == ""
               then do putStrLn "Bye-bye!"
                       Quit ()
               else do putStrLn ("Hello " ++ name)
                       greet

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

run : Fuel -> RunIO a -> IO (Maybe a)
run fuel (Quit value) = pure (Just value)
run Dry p = pure Nothing
run (More fuel) (Do action cont) = do res <- action
                                      run fuel (cont res)                       

partial -- cf above. When you run exec, the fact that you marked total means that any failure of the program to terminate
main : IO () -- is EXCLUSIVELY because you told it to run forever. I.e. it will never get hung up elsewhere on the back end.
main = do run forever greet                                       
          pure ()