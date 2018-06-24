module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
    input <- getLine
    if all isDigit (unpack input)
        then pure (Just (cast input))
        else pure Nothing

guess : (target : Nat) -> (numGuesses: Nat) -> IO ()
guess target numGuesses = 
    do putStrLn ("Guess the number! You're on guess # " ++ (show numGuesses))
       Just numGuess <- readNumber
           | Nothing => do putStrLn "Must be a number ya danged doofus!"
                           guess target numGuesses
       case (compare numGuess target) of
           LT => do putStrLn "Too low :("
                    guess target (S numGuesses)
           GT => do putStrLn "Too high ;)"
                    guess target (S numGuesses)
           EQ => do putStrLn ("You got it! Took ya " ++ (show numGuesses) ++ " guesses.")

main : IO ()
main = do 
    vaguelyRandomInt <- time
    guess (cast {to = Nat} (mod vaguelyRandomInt 100)) Z

myRepl : (prompt : String) -> (onInput : String -> String) -> IO ()
myRepl prompt onInput = 
    do putStr prompt
       input <- getLine
       putStrLn (onInput input)
       myRepl prompt onInput

myReplWith : (state : a) -> (prompt : String ) -> (onInput : String -> Maybe(String,a)) -> IO ()
myReplWith state prompt onInput =
    do putStr prompt
       input <- getLine
       case (onInput input) of
        Just (res,newState) => do putStrLn res
                                  myReplWith newState prompt onInput
        Nothing => putStrLn "bye-bye :("