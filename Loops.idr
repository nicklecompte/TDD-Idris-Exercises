module Main

import System

countDown : (secs : Nat) -> IO ()
countDown Z = putStrLn "let's gooooooooo, boys!"
countDown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countDown secs

readNumber : IO (Maybe Nat)
readNumber = do
    input <- getLine
    if all isDigit (unpack input)
        then pure (Just (cast input))
        else pure Nothing

countDowns : IO ()
countDowns = do putStr "Enter starting number: "
                Just startNum <- readNumber
                     | Nothing => do putStrLn "Invalid input!"
                                     countDowns
                countDown startNum
                putStr "Another countdown? (y/n)  "
                yn <- getLine
                if yn == "y" then countDowns else pure ()