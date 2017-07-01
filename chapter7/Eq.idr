data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

  (/=) x y = not (x == y)

data MyTree elem = Empty
               | Node (MyTree elem) elem (MyTree elem)

Eq elem => Eq (MyTree elem) where
    (==) Empty Empty = True
    (==) (Node left e right) (Node left' e' right') = left == left' && right == right' && e == e'
    (==) _ _ = False
