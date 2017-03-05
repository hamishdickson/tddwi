import Data.Vect

empties : Vect n (Vect 0 elem)
empties = replicate _ []

{-
so interestingly, I didn't really think about this - I just put `zipWith` in here, picked x and xsTrans and then kept
using ctrl-alt-s until all the holes were gone
-}
woozle : (x : Vect n elem) -> (xsTrans : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
woozle x xsTrans = zipWith (\__pi_arg => \__pi_arg6 => (__pi_arg) :: (__pi_arg6)) x xsTrans

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = empties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             woozle x xsTrans


-- add two matricies

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys


-- mult two matricies
-- bleh - this was hard, had to check the answers for hints a bunch of times
-- when doing ctrl-alt-l, find it weird you almost always get an unneeded param

multVecs : Num a => (x : Vect n a) -> (y : Vect n a) -> a
multVecs x y = sum (zipWith (*) x y)

mkRow : Num a => (x : Vect n a) -> (ys : Vect o (Vect n a)) -> Vect o a
mkRow x [] = []
mkRow x (y :: xs) = multVecs x y :: mkRow x xs

multHelper : Num a => (xs : Vect m (Vect n a)) -> (ys : Vect o (Vect n a)) -> Vect m (Vect o a)
multHelper [] ys = []
multHelper (x :: xs) ys = mkRow x ys :: multHelper xs ys

multMatrix : Num a => Vect m (Vect n a) -> Vect n (Vect o a) -> Vect m (Vect o a)
multMatrix xs ys = let ys_trans = transposeMat ys in
                       multHelper xs ys_trans
