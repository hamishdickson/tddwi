module Main

palindrome : String -> Bool
palindrome str = str == reverse str

inSensitivePalindrome : String -> Bool
inSensitivePalindrome str = palindrome (toLower str)

longerThan10 : String -> Bool
longerThan10 str = if (length str > 10) then palindrome str else False

longerThanN : Nat -> String -> Bool
longerThanN n str = if (length str > n) then palindrome str else False

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten ls = take 10 (reverse (sort ls))

over_length : Nat -> List String -> Nat
over_length n ls = length (filter (> n) (map length ls))

show_over_length : String -> String
show_over_length str = show (over_length 2 (words str))

main : IO ()
main = repl "Enter a string: "
            show_over_length
