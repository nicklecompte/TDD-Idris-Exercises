module Main

import Data.Vect

infixr 5 .+.
{-
infixr x opr defines a new operator by fixity and precedence.
infixr x opr means opr is a right-associative infix operator with precendece x (order of ops)
infixl does the same for left-associative
-}

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

addToStore : (store: DataStore) -> (SchemaType (schema store)) -> DataStore       
addToStore (MkData schema size items) str = MkData schema _ (addToData items)
  where
    addToData : Vect oldSize (SchemaType schema) -> Vect (S oldSize) (SchemaType schema)
    addToData [] = [str]
    addToData (x :: xs) = x :: addToData xs

display : SchemaType schema -> String
display {schema = SString} item = item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (itemLeft, itemRight) =  (display itemLeft) ++ "," ++ (display itemRight)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String,DataStore)
getEntry pos store
    = let store_items = items store in
          case integerToFin pos (size store) of
            Nothing => Just ("Out of range :( \n", store)
            Just id => Just(display (index id (items store)) ++ "\n",store)

getAll : (store : DataStore) -> Maybe (String,DataStore)
getAll store
    = Just ( concat (map (\x => (display x) ++ "\n") (items store) ), store)

data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Maybe Integer -> Command schema
  Quit : Command schema            


parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs)
  = case xs of
        [] => Just SString
        _ => do xs_schema <- parseSchema xs
                Just (SString .+. xs_schema)
parseSchema ("Int" :: xs)                  
  = case xs of
         [] => Just SInt
         _ => do xs_schema <- parseSchema xs
                 Just (SInt .+. xs_schema)
parseSchema ("Char" :: xs)
  = case xs of
          [] => Just SChar
          _ => do xs_schema <- parseSchema xs
                  Just (SChar .+. xs_schema)                  

parseSchema _ = Nothing                  

stringToSchemaPrefixHelper : (schema : Schema) -> String -> Maybe(SchemaType schema, String) -- parses the first part of an input and returns the rest of the string if it works
stringToSchemaPrefixHelper SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe(String, String)
    getQuoted ('"' :: xs)
      = case span (/= '"') xs of
              (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
              _ => Nothing
    getQuoted _ = Nothing
stringToSchemaPrefixHelper SInt input = case span isDigit input of
                                      ("",remainder) => Nothing -- if prefix of span is empty when checking isDigit, then there aren't any numbers in the input
                                      (num,remainder) => Just(cast num, ltrim (remainder))
stringToSchemaPrefixHelper SChar input = let (li,remainder) = span (/= ' ') input in
                                              case (unpack li) of
                                                    (x :: []) => Just (x, ltrim(remainder))
                                                    _ => Nothing
stringToSchemaPrefixHelper (schemaX .+. schemaY) input = do (xValu, input') <- stringToSchemaPrefixHelper schemaX input
                                                            (yValu, input'') <- stringToSchemaPrefixHelper schemaY input'
                                                            Just((xValu,yValu),input'')

stringToSchema : (schema : Schema) -> (str : String)  -> Maybe (SchemaType schema) -- could fail and return Nothing
stringToSchema schema str = case (stringToSchemaPrefixHelper schema str) of
                                 Just (parsedSchemaType, "") => Just parsedSchemaType -- if it parses correctly and returns an empty string, 
                                                                                      -- then stringToSchema worked and we return the parsed SchemaType
                                 Just _ => Nothing -- parsed worked and the parsedSchemaType is what we want. But the result string is NOT empty - so there was more data than assumed by the schema and this fails.
                                 Nothing => Nothing -- parsing failed entirely                                                                                      

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" str = do parsedOk <- (stringToSchema schema str)
                                   Just (Add parsedOk)
parseCommand schema "get" "" = Just (Get Nothing)                                      
parseCommand schema "get" val = case all isDigit (unpack val) of -- all : (pred: a -> Bool) -> List a -> bool checks if pred is satisifed on ervy elt of list. isDigit : Char -> bool if the char is a digit. unPack : String -> List char
                              False => Nothing
                              True => Just (Get (Just (cast val)))
-- parseCommand schema "size" "" = Just Size
parseCommand schema "schema" defn = case parseSchema (words defn) of
                                        Nothing => Nothing
                                        Just schema' => Just (SetSchema schema')
                                        
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input:String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of -- we want the datastore to be empty if we're updating the schema
                              Z => Just (MkData schema _ [])                           
                              _ => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse (schema store) inp of
                              Nothing => Just ("Invalid command \n", store)
                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Get Nothing) => getAll store
                              Just (Get (Just pos)) => getEntry pos store
                              Just (SetSchema schema) => case setSchema store schema of
                                                              Nothing => Just ("Can't update schema :( here's the old store \n", store)
                                                              Just newStore => Just ("Set schema. \n", newStore)
                              Just Quit => Nothing


main : IO ()                              
main = replWith (MkData SString _ []) "Command: " processInput