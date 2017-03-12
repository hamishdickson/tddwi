data Vector : Nat -> Type -> Type where
     Nil    : Vector Z a
     (::)   : (x : a) -> (xs : Vector k a) -> Vector (S k) a

%name Vector xs, ys, zs

append : Vector n elem -> Vector m elem -> Vector (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vector n a -> Vector n b -> Vector n (a, b)
zip [] ys = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

--index : Fin n -> Vector n a -> a
