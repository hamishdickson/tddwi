{-
Idris requires that functions be defined before their use, but when you have two
functions that require one another, you can use the mutual block to get around this
-}
mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k
