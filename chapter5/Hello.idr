module Main

printLength : IO ()
printLength = getLine >>= \input => let len = length input in
                                        putStrLn (show len)

printWordLength : IO ()
printWordLength = putStr "Input string: " >>= \_ =>
                  getLine >>= \input =>
                  let len = length input in
                  putStrLn (show len)

main : IO ()
main = do
     putStr "Enter your name: "
     x <- getLine
     putStrLn ("Hello " ++ x ++ "!")
