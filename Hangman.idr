import Data.Vect

removeElem_f : DecEq a => (value : a) -> 
    (y : Vect (S n) a) -> 
    {auto prf: Elem value y} ->
    Vect n a
removeElem_f value (value :: xs) {prf = Here} = xs
removeElem_f {n = Z} value (x :: []) {prf = (There later)} = absurd later
removeElem_f {n = (S k)} value (x :: xs) {prf = (There later)} = x :: (removeElem_f value xs)

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
    MkWordState : (word : String) -> 
                  (missing : Vect letters Char) ->
                  WordState guesses_remaining letters

data Finished : Type where
    Lost : (game : WordState 0 (S letters)) -> Finished
    Won : (game : WordState (S guesses) 0) -> Finished

data ValidInput : List Char -> Type where
    Letter : (c: Char) -> ValidInput [c]

emptyIsNotValid : ValidInput [] -> Void
emptyIsNotValid x impossible

longerThanOneIsNotValid: ValidInput (x::(y::xs)) -> Void
longerThanOneIsNotValid x impossible

isValidInput : (cs: List Char) -> Dec (ValidInput cs)
isValidInput [] = No (emptyIsNotValid)
isValidInput (x::[]) = Yes (Letter x)
isValidInput (x::(y::xs)) = No (longerThanOneIsNotValid)

isValidInputString : (s:String) -> Dec (ValidInput (unpack s))
isValidInputString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess:"
               x <- getLine
               case isValidInputString (toUpper x) of
                (Yes prf) => pure (_ ** prf)
                (No contra) => do putStrLn "Invalid guess :("
                                  readGuess

processGuess : (letter : Char) ->
               (WordState (S guesses) (S letters)) ->
               Either (WordState guesses (S letters))
                      (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) = case isElem letter missing of
                                                      (Yes prf) => Right (MkWordState word (removeElem_f letter missing))
                                                      (No contra) => Left (MkWordState word missing)



game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st
    = do (_ ** Letter letter) <- readGuess
         case processGuess letter st of
            Left wr => do putStrLn "Wrong!"
                          case guesses of
                            Z => pure (Lost wr)
                            S k => game wr
            Right ri => do putStrLn "Correct!"
                           case letters of
                             Z => pure (Won ri)
                             S k => game ri


main : IO ()
main = do result <- game {guesses=2} (MkWordState "Test" ['T','E','S'])                             
          case result of
                Lost (MkWordState word missing) => putStrLn ("You lose. The word was " ++ word)
                Won game => putStrLn "You win!"