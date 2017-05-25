module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

{-
data DataStore : Type where
     MkData : (schema : Schema) ->
              (size : Nat) ->
              (items : Vect size (SchemaType schema)) ->
              DataStore


size : DataStore -> Nat
size (MkData schema' size' items') = size'

schema : DataStore -> Schema
schema (MkData schema' size' items') = schema'

items : (store : DataStore) -> Vect (size store) (SchemaType (schema store))
items (MkData schema' size' items') = items'
-}

{-
this is a record - which is a way of automatically deriving the above projections

this replaces the data type defined above and it's projections

*DataStore> :doc DataStore
Record DataStore

Constructor:
    MkData : (schema : Schema) ->
        (size : Nat) -> (items : Vect size (SchemaType schema)) -> DataStore


Projections:
    schema : (rec : DataStore) -> Schema


    size : (rec : DataStore) -> Nat


    items : (rec : DataStore) -> Vect (size rec) (SchemaType (schema rec))

-}
record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)



{-
size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items) where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs tot inp = let val = cast inp in
                        if val < 0
                           then Nothing
                           else let newVal = tot + val in
                                    Just("Subtotal: " ++ show newVal ++ "\n", newVal)

data Command = Add String
             | Get Integer
             | Quit
             | Size
             | Search String

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "quit" _ = Just Quit
parseCommand "size" _ = Just Size
parseCommand "search" str = Just (Search str)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                              case integerToFin pos (size store) of
                                    Nothing => Just ("Out of range\n", store)
                                    Just id => Just (index id store_items ++ "\n", store)

searchString : Nat -> (items : Vect n String) -> (str : String) -> String
searchString idx [] str = ""
searchString idx (x :: xs) str
            = let rest = searchString (idx + 1) xs str in
                      if isInfixOf str x
                          then show idx ++ ": " ++ x ++ "\n" ++ rest
                          else rest

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Get item) => getEntry item store
                              Just Quit => Nothing
                              Just Size => Just ("Size " ++ show (size store)"\n", store)
                              Just (Search str) => Just (searchString 0 (items store) str, store)

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
-}
