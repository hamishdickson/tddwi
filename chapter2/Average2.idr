module Average2

||| Calculate the average length of words in a string
||| You can see this by typing `:doc average` in the repl
||| @str does stuff
export
average : (str : String) -> Double
average str = let numWords = wordCount str
                  totalLength = sum (allLengths (words str)) in

                  cast totalLength / cast numWords

  where
    wordCount : String -> Nat
    wordCount str = length (words str)

    allLengths : List String -> List Nat
    allLengths strs = map length strs
