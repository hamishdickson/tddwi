module Main

printLength : IO ()
printLength = getLine >>= \input => let len = length input in
                                        putStrLn (show len)

printLength2 : IO ()
printLength2 = do putStr "Input string: "
                  input <- getLine
                  let len = length input
                  putStrLn (show len)

pickOne : (input1 : String) -> (input2 : String) -> String
pickOne input1 input2 = case (length input1) > (length input2) of
                             False => "length " ++ show (length input2)
                             True => "length " ++ show (length input1)

printLonger : IO ()
printLonger = do putStr "Input String: "
                 input1 <- getLine
                 putStr "And another: "
                 input2 <- getLine
                 putStrLn (pickOne input1 input2)

printLonger2 : IO ()
printLonger2 = putStr "one" >>= \_ => getLine >>= \i1 => putStr "two" >>= \_ => getLine >>= \i2 => putStrLn (pickOne i1 i2)



printWordLength : IO ()
printWordLength = putStr "Input string: " >>= \_ =>
                  getLine >>= \input =>
                  let len = length input in
                  putStrLn (show len)

printTwoThings : IO ()
printTwoThings = do putStr "Hello"
                    putStrLn "World"


main : IO ()
main = do
     putStr "Enter your name: "
     x <- getLine
     putStrLn ("Hello " ++ x ++ "!")
