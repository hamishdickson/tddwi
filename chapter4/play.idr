import Data.Vect

myAppend : Vect n a -> Vect m a -> Vect (n + m) a
myAppend [] ys = ys
myAppend (x :: xs) ys = x :: myAppend xs ys
