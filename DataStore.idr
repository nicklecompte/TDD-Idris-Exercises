module Main

import Data.Vect

data DataStore : Type where
  MKData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MKData size' items') = size' -- this syntax isn't necessary
-- this is basically a record type and Idris has sugar for that
-- kinda like in C# though, let's do some getters with private backing fields :)

items : (store: DataStore) -> Vect (size store) String -- we project the size of the store out of the input and type it to the output
items (MKData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MKData size items) str = MKData (S size) (str :: items)

data Command = Add String | Get Integer | Size | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of -- all : (pred: a -> Bool) -> List a -> bool checks if pred is satisifed on ervy elt of list. isDigit : Char -> bool if the char is a digit. unPack : String -> List char
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input:String) -> Maybe Command
parse input = case (span (/= ' ') input) of -- span splits the string where the first character in the lambda fails
                   (cmd,args) => parseCommand cmd (ltrim args) -- ltrim removes leading whitespace

processCommand : (cmd : Command) -> (store : DataStore) -> Maybe (String, DataStore)
processCommand (Add item) store = Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
processCommand (Get index) store = case (integerToFin index (size store)) of
                                    Nothing => Just ("Out of range :( \n", store)
                                    Just id => let item = (Data.Vect.index id (items store)) in
                                                   Just ("Item is: " ++ item ++ "\n", store)
processCommand (Size) store = Just ("Size is " ++ (show (size store)) ++ "\n",store)
processCommand Quit store = Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command \n", store)
                              Just cmd => processCommand cmd store


main : IO ()
main = replWith (MKData _ []) "Command:" processInput
