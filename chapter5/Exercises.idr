module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

guess : (target : Nat) -> IO ()
guess target =
  do Just num <- readNumber | Nothing => putStrLn "NaN"
     case compare num target of
       EQ => putStrLn "Got it!"
       GT => putStrLn "Too high" >>= \_ => guess target
       LT => putStrLn "Too low" >>= \_ => guess target


guess2 : (target : Nat) -> (guesses : Nat) -> IO ()
guess2 target guesses =
  do Just num <- readNumber | Nothing => putStrLn "NaN"
     case compare num target of
       EQ => putStrLn ("Got it.. and it only took you " ++ (show (guesses + 1)) ++ " guesses")
       GT => putStrLn "Too high" >>= \_ => guess2 target (guesses + 1)
       LT => putStrLn "Too low" >>= \_ => guess2 target (guesses + 1)

main : IO ()
main = do t <- time
          guess2 (cast (t `mod` 101)) 0
