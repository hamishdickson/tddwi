data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)


insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => Node left val right
                                      GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ val :: (treeToList right)

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Sub x y) = (evaluate x) - (evaluate y)
evaluate (Mult x y) = (evaluate x) * (evaluate y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = if (x > y) then Just x else Just y

-- need all this for biggestTriange

data Shape = ||| Triangle with a base and height length
             Triangle Double Double
           | ||| Rectangle with width and height
             Rectangle Double Double
           | ||| Circle with radius
             Circle Double


data Picture = Primitive Shape
            | Combine Picture Picture
            | Rotate Double Picture
            | Translate Double Double Picture

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle width height) = width * height
area (Circle radius) = pi * radius * radius

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive (Triangle x y)) = Just (area (Triangle x y))
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine x y) = maxMaybe (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate x y) = biggestTriangle y
biggestTriangle (Translate x y z) = biggestTriangle z


testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))
testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))
