import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

-- dependentPairs
anyVect : (n : Nat ** Vect n String)                       
anyVect = (3 ** ["Rod","Jane","Fred"])

-- we can use this to improve readVectLen by not making the caller supply the length of the vect ahead of time
readVect : IO (len ** Vect len String) -- don't need to say len : Nat since compiler infers that from Vect len
readVect = do x <- getLine
              if (x == "")
                then pure (Z ** []) -- don't need to put Z, compiler can infer it if you do _
                else do (_ ** xs) <- readVect
                        pure (_ ** x :: xs)

{-

Write a program that:
-- read two input vectors
-- if they have different lengths, display an error
-- if they have the same lengths, zip them and display the results    

-}                        

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (enter a blank line to terminate input)"
               (len1 ** vec1) <- readVect
               putStrLn "Enter the second vector:"
               (len2 ** vec2) <- readVect
               case exactLength len1 vec2 of
                    Nothing => putStrLn "Error, vectors had different lengths"
                    Just vec2' => printLn (zip vec1 vec2')-- we have guaranteed that length vec2' equals vec1 and the values = vec2
