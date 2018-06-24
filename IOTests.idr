printLengthLambda : IO ()
printLengthLambda = getLine >>= \input => let len = length input in
                                              putStrLn (show len)

printTwoThings : IO ()
printTwoThings = do putStrLn "thing 1"                                        
                    putStrLn "thing 2"

printLength : IO ()                    
printLength = do putStr "Input string: "
                 input <- getLine
                 let len = length input
                 putStrLn (show len)

||| Takes in two input strings and prints the length of the longer one

printLonger : IO ()
printLonger = do putStr "First input string: "        
                 input1 <- getLine
                 let len1 = length input1
                 putStr "Second input string: "
                 input2 <- getLine
                 let len2 = length input2
                 putStrLn ("Longer string has length: " ++ (show (max len1 len2)))

printLongerCombinator : IO ()
printLongerCombinator = putStr "First input string: "
                        >>= \_ => getLine 
                        >>= \firstInput => let len1 = length firstInput in
                                               putStr "Second input string: "
                                               >>= \_ => getLine >>= \secondInput => let len2 = length secondInput in
                                                                                         putStrLn ("Longer string has length " ++ (show (max len1 len2)))