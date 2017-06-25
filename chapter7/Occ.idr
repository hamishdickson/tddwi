data Phases = Solid | Liquid | Gas | Plasma

Eq Phases where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) Plasma Plasma = True
  (==) _ _ = False

  (/=) x y = not (x == y)

occur : Eq ty => (item : ty) -> (values : List ty) -> Nat
occur item [] = 0
occur item (x :: xs) = case x == item of
                            False => occur item xs
                            True => 1 + occur item xs

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
