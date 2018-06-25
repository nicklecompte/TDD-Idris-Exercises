||| Reads input from the console until the user enters a blank line                    
readToBlank : IO (List String)                    
readToBlank = do putStrLn "Enter some a line (blank to end): "
                 input <- getLine
                 if (input == "")
                    then pure []
                    else do inputTail <- readToBlank
                            pure (input :: inputTail)

||| Reads input from the console until user enters a blank line,
||| then reads a filename from the console and writes input to that file.
readAndSave : IO ()
readAndSave = do listData <- readToBlank
                 putStrLn "Save to which file? "
                 fileName <- getLine
                 Right _ <- do writeFile fileName (listStringToOutput listData) -- creates file if not exists
                    | Left err => do putStrLn (show err)
                 putStrLn "done"
    where
        listStringToOutput : (l: List String) -> String
        listStringToOutput [] = ""
        listStringToOutput (x::xs) = (show x) ++ "\n" ++ (listStringToOutput xs)