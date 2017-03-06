data DivResult = DivByZero | Result Double

safeDivide : Double -> Double -> DivResult
safeDivide x y = if (y == 0) then DivByZero else Result (x / y)


data MyMaybe valtype =
             Nothing
           | Just valtype

safeDivide2 : Double -> Double -> MyMaybe Double
safeDivide2 x y = if (y == 0) then Nothing else Just (x / y)

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

-- binary search tree
insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => Node left val right
                                      GT => Node left val (insert x right)


-- this and compare are in Prelude
data MyOrdering = LT | EQ | GT

myCompare : Ord a => a -> a -> MyOrdering
